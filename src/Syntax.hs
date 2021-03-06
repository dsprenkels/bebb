{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import AST
import Control.Arrow (left)
import Error
import RIO hiding (many, some, try)
import RIO.Char (isAsciiLower, isAsciiUpper, isDigit)
import RIO.Text (append, pack, singleton)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Our custom Megaparsec parser type
type Parser = Parsec Void Text

nodeParser :: Node n => Parser a -> Parser (n a)
nodeParser parser = do
  lo <- getOffset
  node <- parser
  hi <- getOffset
  return $ newNode node (lo, hi)

-- | Parse an assembly source file and return an AST
parse :: String -> Text -> Either ErrorBundle (AST WithPos)
parse filename text = left fromParseErrorBundle (runParser pASM filename text)

-- | Consume line comments
lineComment :: Parser ()
lineComment = L.skipLineComment ";"

-- | Consume space characters (including newlines)
scn :: Parser ()
scn = L.space space1 lineComment empty

-- | Consume space characters but not newlines
sc :: Parser ()
sc = L.space (void (char ' ' <|> char '\t')) lineComment empty

-- | Custom version of Text.Megaparsec.Char.Lexer.symbol
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Assembly source parser
pASM :: Node a => Parser (AST a)
pASM = (concat <$> pLine `sepBy` char '\n') <* scn <* eof

-- | Parse a line
pLine :: Node a => Parser [Decl a]
pLine = do
  sc
  maybeBothNode <- optional $ nodeParser pLabelOrMnemonic
  case maybeBothNode of
    Just bothNode -> do
      sc
      let nameNode = nodeMap fst bothNode
      let isLabel = snd $ unpackNode bothNode
      let dataSize = dataSizeFromText (unpackNode nameNode)
      case (isLabel, dataSize) of
        (True, _) -> do
          rest <- pLine
          return (LblDecl nameNode : rest)
        (False, Just dataSize') -> do
          let dataSizeN = nodeMap (const dataSize') nameNode
          dataDecl <- pDataDecl dataSizeN <?> "data"
          return [dataDecl]
        (False, Nothing) -> do
          instr <- nodeParser (pInstruction nameNode) <?> "instruction"
          return [InstrDecl instr]
    Nothing -> return []

-- | Parse a label or instruction mnemonic
-- |
-- | Returns (name, isLabel)
pLabelOrMnemonic :: Parser (Label, Bool)
pLabelOrMnemonic = do
  name <- pLabel
  sc
  colon <- optional $ char ':'
  return (name, isJust colon)

-- | Parse a label declaration
pLabelDecl :: Node a => Parser (Decl a)
pLabelDecl = (LblDecl <$> nodeParser pLabel) <* symbol ":"

-- | Parse a DataSize from Text
dataSizeFromText :: Text -> Maybe DataSize
dataSizeFromText "i8" = Just I8
dataSizeFromText "i16" = Just I16
dataSizeFromText "i32" = Just I32
dataSizeFromText "i64" = Just I64
dataSizeFromText _ = Nothing

-- | Parse the declaration of literal data
-- |
-- | Takes a Text node that should contain the parsed data size.
pDataDecl :: Node a => a DataSize -> Parser (Decl a)
pDataDecl dataSize = DataDecl dataSize <$> nodeParser pExpr `sepBy1` symbol ","

-- | Parse an instruction
pInstruction :: Node a => a Text -> Parser (Instruction a)
pInstruction mnemonic = do
  opnds <- nodeParser pAnyOp `sepBy` symbol ","
  return $ Instr {mnemonic, opnds}
  where
    pAnyOp = (pImmOp <|> pAddrOp) <?> "instruction operand"
    pImmOp = Imm <$> (symbol "#" *> pExpr)
    pAddrOp = Addr <$> pExpr

-- | Parse an instruction mnemonic
pMnemonic :: Parser Text
pMnemonic = pLabel <?> "mnemonic"

-- | Wrap a parser between parentheses ("( ... )")
parens :: Parser a -> Parser a
parens = symbol "(" `between` symbol ")"

-- | Parse an expression
pExpr :: Node a => Parser (Expr a)
pExpr = pBinOpExpr binaryOps <?> "expression"

-- | Parse an expression, but restrict to parsing the binary operations in `opsLeft`
pExpr' :: Node a => [BinaryOps] -> Parser (Expr a)
pExpr' [] = pTermExpr
pExpr' opsLeft = pBinOpExpr opsLeft

-- | A list of binary operations seperated by some symbol
type BinaryOps = [(Text, BinOp)]

-- | Parse a binary expression ('3 + 3')
pBinOpExpr :: Node a => [BinaryOps] -> Parser (Expr a)
pBinOpExpr [] = pTermExpr
pBinOpExpr (ops:nextLevelOps) = do
  ret <- nodeParser $ pExpr' nextLevelOps
  rest <-
    many $ do
      op <- pOp ops <?> "binary operator"
      rhs <- nodeParser $ pExpr' nextLevelOps
      return (op, rhs)
  return $ unflatten ret rest
  -- unflatten is left-associative
  where
    unflatten root [] = unpackNode root
    unflatten lhs ((op, rhs):rest) = unflatten (nodeFrom (lhs, rhs) (Binary op lhs rhs)) rest

-- | Definitions of all binary operations ordered by precedence.
binaryOps :: [BinaryOps]
binaryOps =
  [ [("&", And), ("|", Or), ("^", Xor)]
  , [("+", Add), ("-", Sub)]
  , [("*", Mul), ("/", Div), ("%", Mod)]
  ]

-- | Parse a unary expression ('-3')
pUnOpExpr :: Node a => Parser (Expr a)
pUnOpExpr = do
  op <- pOp unaryOps <?> "unary operator"
  opnd <- nodeParser pExpr
  return $ Unary op opnd

-- | A list of unary operations prefixed by some symbol
type UnaryOps = [(Text, UnOp)]

-- | Definitions of all unary operations.
unaryOps :: UnaryOps
unaryOps = [("+", Pos), ("-", Neg), ("~", Not)]

-- | Parse an operator chosen from a list of operators
pOp :: [(Text, a)] -> Parser a
pOp [] = empty
pOp ((sym, op):next) = (symbol sym >> return op) <|> pOp next

-- | Parse an expression
pTermExpr :: Node a => Parser (Expr a)
pTermExpr = pUnOpExpr <|> pParenExpr <|> pIdentExpr <|> pLitExpr

-- | Parse an expression between parentheses
pParenExpr :: Node a => Parser (Expr a)
pParenExpr = parens pExpr

-- | Parse an identifier
pIdentExpr :: Node a => Parser (Expr a)
pIdentExpr = Ident <$> nodeParser pLabel <* sc

-- | Parse a literal (number)
pLitExpr :: Node a => Parser (Expr a)
pLitExpr = Lit <$> nodeParser pNumber <* sc

-- | Parse a number
pNumber :: Parser Int64
pNumber = pHexadecimal <|> pBinary <|> pDecimal

-- | Parse a decimal value ("42")
pDecimal :: Parser Int64
pDecimal = L.signed sc L.decimal <?> "decimal value"

-- | Parse a hexadecimal value ("0x2A")
pHexadecimal :: Parser Int64
pHexadecimal = (string' "0x" *> L.hexadecimal) <?> "hex value"

-- | Parse a binary value ("0b101010")
pBinary :: Parser Int64
pBinary = (string' "0b" *> L.binary) <?> "binary value"

-- | Parse a label identifier ("_start", ".loop1", etc.)
pLabel :: Parser Label
pLabel = pLabel' <?> "label"
  where
    pLabel' = do
      p <- fromMaybe "" <$> maybeDot
      p' <- append p <$> (singleton <$> fstLetter)
      append p' . pack <$> many otherLetter
    maybeDot = (try . optional) (singleton <$> char '.')
    fstLetter = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_'
    otherLetter = fstLetter <|> satisfy isDigit
