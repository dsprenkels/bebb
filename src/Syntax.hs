{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

-- TODO(dsprenkels): Add a custom error reporting function, that not only
-- supports parse errors, but also type errors etc.
--
module Syntax where

import           AST
import           Error
import           RIO                     hiding ( many
                                                , some
                                                , try
                                                )
import           RIO.Char                       ( isAlpha
                                                , isAlphaNum
                                                , isAsciiLower
                                                , isAsciiUpper
                                                , isDigit
                                                )
import           RIO.Text                       ( append
                                                , pack
                                                , singleton
                                                )
import           Text.Megaparsec         hiding ( parse )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Arrow                  ( left )

-- | Our custom Megaparsec parser type
type Parser = Parsec Void Text

data SourceSpan = SourceSpan
    { lo :: Int
    , hi :: Int
    }
    deriving (Show, Eq)

class Node (n :: * -> *) where
  nodeParser :: Parser a -> Parser (n a)
  nodeFrom ::  (n b, n c) -> a -> n a
  unpackNode :: n a -> a

data WithPos a = WP
    { node :: a
    , ss   :: SourceSpan
    }
    deriving (Show, Eq)

instance Node WithPos where
  nodeParser parser = do
    lo   <- getOffset
    node <- parser
    hi   <- getOffset
    return WP { node, ss = SourceSpan { lo, hi } }
  nodeFrom (n1, n2) node = WP { node, ss = SourceSpan { lo, hi } }
   where
    WP { ss = SourceSpan { lo } } = n1
    WP { ss = SourceSpan { hi } } = n2
  unpackNode WP { node } = node

deriving instance Show (Decl WithPos)

deriving instance Eq (Decl WithPos)

deriving instance Show (Instruction WithPos)

deriving instance Eq (Instruction WithPos)

deriving instance Show (Operand WithPos)

deriving instance Eq (Operand WithPos)

deriving instance Show (Expr WithPos)

deriving instance Eq (Expr WithPos)


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

-- | Lex a lexeme with spaces
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Custom version of Text.Megaparsec.Char.Lexer.symbol
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Custom version of Text.Megaparsec.Char.Lexer.symbol'
symbol' :: Text -> Parser Text
symbol' = L.symbol' sc

-- | Assembly source parser
pASM :: Node a => Parser (AST a)
pASM = (concat <$> many pLine) <* eof

-- | Parse a line
pLine :: Node a => Parser (AST a)
pLine = do
  sc
  lbl   <- many (try pLabelDecl) <* sc
  decls <- (pure <$> pInstructionDecl <* sc) <|> (pDataDecl `sepBy` symbol ",")
  void (char '\n')
  return (lbl ++ decls)

-- | Parse a label declaration
pLabelDecl :: Node a => Parser (Decl a)
pLabelDecl = (LblDecl <$> nodeParser pLabel) <* symbol ":"

-- | Parse an instruction declaration (i.e. a line containing an instruction)
pInstructionDecl :: Node a => Parser (Decl a)
pInstructionDecl = InstrDecl <$> nodeParser pInstruction

-- | Parse the declaration of literal data
pDataDecl :: Node a => Parser (Decl a)
pDataDecl = DataDecl <$> nodeParser pExpr

-- | Parse an instruction
pInstruction :: Node a => Parser (Instruction a)
pInstruction = do
  mnemonic <- nodeParser pMnemonic
  opnds    <- nodeParser pAnyOp `sepBy` symbol ","
  return $ Instr { mnemonic, opnds }
 where
  pAnyOp  = (pImmOp <|> pAddrOp) <?> "instruction operand"
  pImmOp  = Imm <$> (symbol "#" *> pExpr)
  pAddrOp = Addr <$> pExpr

-- | Parse an instruction mnemonic
pMnemonic :: Parser Text
pMnemonic = lexeme (pName <?> "mnemonic")

-- | Wrap a parser between parentheses ("( ... )")
parens :: Parser a -> Parser a
parens = symbol "(" `between` symbol ")"

-- | Wrap a parser between brackets ("[ ... ]")
brackets :: Parser a -> Parser a
brackets = symbol "[" `between` symbol "]"

-- | Parse an address ("0x2A2A")
pAddress :: Node a => Parser (Operand a)
pAddress = (Addr <$> pExpr) <?> "address"

-- | Parse an expression
pExpr :: Node a => Parser (Expr a)
pExpr = pBinOpExpr binaryOps <?> "expression"

-- | Parse an expression, but restrict to parsing the binary operations in `opsLeft`
pExpr' :: Node a => [BinaryOps] -> Parser (Expr a)
pExpr' []      = pTermExpr
pExpr' opsLeft = pBinOpExpr opsLeft

-- | A list of binary operations seperated by some symbol
type BinaryOps = [(Text, BinOp)]

-- | Parse a binary expression ('3 + 3')
pBinOpExpr :: Node a => [BinaryOps] -> Parser (Expr a)
pBinOpExpr []                   = pTermExpr
pBinOpExpr (ops : nextLevelOps) = do
  ret  <- nodeParser $ pExpr' nextLevelOps
  rest <- many $ do
    op  <- pOp ops
    rhs <- nodeParser $ pExpr' nextLevelOps
    return (op, rhs)
  return $ unflatten ret rest
 where
  -- unflatten is left-associative
  unflatten root [] = unpackNode root
  unflatten lhs ((op, rhs) : rest) =
    unflatten (nodeFrom (lhs, rhs) (Binary op lhs rhs)) rest

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
  op   <- pOp unaryOps
  opnd <- nodeParser pExpr
  return $ Unary op opnd

-- | A list of unary operations prefixed by some symbol
type UnaryOps = [(Text, UnOp)]

-- | Definitions of all unary operations.
unaryOps :: UnaryOps
unaryOps = [("+", Pos), ("-", Neg), ("~", Not)]

-- | Parse an operator chosen from a list of operators
pOp :: [(Text, a)] -> Parser a
pOp []                 = empty
pOp ((sym, op) : next) = (symbol sym >> return op) <|> pOp next

-- | Parse an expression
pTermExpr :: Node a => Parser (Expr a)
pTermExpr = pUnOpExpr <|> pParenExpr <|> pIdentExpr <|> pLitExpr

-- | Parse an expression between parentheses
pParenExpr :: Node a => Parser (Expr a)
pParenExpr = parens pExpr

-- | Parse an identifier
pIdentExpr :: Node a => Parser (Expr a)
pIdentExpr = Ident <$> nodeParser pLabel

-- | Parse a literal (number)
pLitExpr :: Node a => Parser (Expr a)
pLitExpr = Lit <$> nodeParser pNumber

-- | Parse an address operand ("0x2A2A")
pName :: Parser Text
pName = do
  c    <- satisfy isAlpha
  rest <- many $ satisfy isAlphaNum
  return $ pack $ c : rest

-- | Parse a number
pNumber :: Parser Int
pNumber = pHexadecimal <|> pBinary <|> pDecimal

-- | Parse a decimal value ("42")
pDecimal :: Parser Int
pDecimal = lexeme (L.signed sc L.decimal) <?> "decimal value"

-- | Parse a hexadecimal value ("0x2A")
pHexadecimal :: Parser Int
pHexadecimal = lexeme (string' "0x" *> L.hexadecimal) <?> "hex value"

-- | Parse a binary value ("0b101010")
pBinary :: Parser Int
pBinary = lexeme (string' "0b" *> L.binary) <?> "binary value"

-- | Parse a label identifier ("_start", ".loop1", etc.)
pLabel :: Parser Label
pLabel = lexeme (pLabel' <?> "label")
 where
  pLabel' = do
    p  <- fromMaybe "" <$> maybeDot
    p' <- append p <$> (singleton <$> fstLetter)
    append p' . pack <$> many otherLetter
  maybeDot    = (try . optional) (singleton <$> char '.')
  fstLetter   = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_'
  otherLetter = fstLetter <|> satisfy isDigit


