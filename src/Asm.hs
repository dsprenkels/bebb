{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO(dsprenkels): Add a custom error reporting function, that not only
-- supports parse errors, but also type errors etc.
--
-- TODO(dsprenkels): Do not "parse" instructions and opcodes immediately, but
-- parse instructions as-is, and later check if they are correct.
--
-- TODO(dsprenkels): Declare some kind of ASTNode a = Span a
--
-- TODO(dsprenkels): Put AST definition in AST.hs
--
module Asm where

import RIO hiding (many, some, try)
import RIO.Char (isAsciiLower, isAsciiUpper, isDigit)
import RIO.Text (append, pack, singleton)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)

data Register
  = PC
  | SP
  deriving (Show, Eq)

data Span =
  Span
    { lo :: SourcePos
    , hi :: SourcePos
    }
  deriving (Show, Eq)

data Address
  = Lbl Label
  | LitAddr Word16
  deriving (Show, Eq)

newtype ShortAddress =
  LitShortAddr Word8
  deriving (Show, Eq)

newtype Immediate =
  Imm Word8
  deriving (Show, Eq)

type Label = Text

data Instruction
  = LDA Address
  | LDAB ShortAddress
  | LDAI Immediate
  | STA Address
  | STAB ShortAddress
  | ADD ShortAddress
  | ADDI Immediate
  | ADC ShortAddress
  | ADCI Immediate
  | SUB ShortAddress
  | SUBI Immediate
  | SUBC ShortAddress
  | SUBCI Immediate
  | OR ShortAddress
  | ORI Immediate
  | AND ShortAddress
  | ANDI Immediate
  | XOR ShortAddress
  | XORI Immediate
  | JMP Address
  | JZ Address
  | RET
  deriving (Show, Eq)

data Decl
  = InstrDecl Instruction
  | LblDecl Address
  deriving (Show, Eq)

type AST = [Decl]

type Parser = Parsec Void Text

-- | Assembly the contents of an assembly file to binary
assemble :: Text -> ByteString
assemble _ = error "unimplemented"

-- | Consume line comments
lineComment :: Parser ()
lineComment = L.skipLineComment ";"

-- | Consume space characters (including newlines)
scn :: Parser ()
scn = L.space space1 lineComment empty

-- | Consume space characters but not newlines
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

-- | Lex a lexeme with spaces
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Custom version of Text.Megaparsec.Char.Lexer.symbol
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Custom version of Text.Megaparsec.Char.Lexer.symbol'
symbol' :: Text -> Parser Text
symbol' = L.symbol' sc

-- | Parse a DM assmebly source file
pASM :: Parser AST
pASM = concat <$> many pLine <* eof

-- | Parse a line
pLine :: Parser [Decl]
pLine = (maybeToList <$> optional pDecl) <* sc <* char '\n'

-- | Parse a general declaration
pDecl :: Parser Decl
pDecl = try pLabelDecl <|> try pInstructionDecl

-- | Parse a label declaration
pLabelDecl :: Parser Decl
pLabelDecl = (LblDecl <$> pAddressExpr) <* symbol ":"

-- | Parse an instruction declaration (i.e. a line containing an instruction)
pInstructionDecl :: Parser Decl
pInstructionDecl = do
  _ <- some (char ' ' <|> char '\t') -- Require indentation
  InstrDecl <$> pInstruction

-- | Parse an instruction
pInstruction :: Parser Instruction
pInstruction =
  choice
    [ try $ do
        _ <- try $ symbol' "LDA"
        (LDAB <$> try pShortAddress) <|> (LDA <$> try pAddressExpr) <|>
          (LDAI <$> try pImmediate)
    , try $ do
        _ <- try $ symbol' "STA"
        (STAB <$> try pShortAddress) <|> (STA <$> try pAddressExpr)
    , try $ pOpSAddrImm "ADD" ADD ADDI
    , try $ pOpSAddrImm "ADC" ADC ADCI
    , try $ pOpSAddrImm "SUB" SUB SUBI
    , try $ pOpSAddrImm "SUBC" SUBC SUBCI
    , try $ pOpSAddrImm "OR" OR ORI
    , try $ pOpSAddrImm "AND" AND ANDI
    , try $ pOpSAddrImm "XOR" XOR XORI
    , try $ pOpAddr "JMP" JMP
    , try $ pOpAddr "JZ" JZ
    , try $ symbol' "RET" >> return RET
    ] <?>
  "instruction"
  where
    pOpAddr opcode ctr
      -- Parse an instruction that has a short address or an immediate operand.
     = do
      _ <- try $ symbol' opcode
      ctr <$> try pAddressExpr
    pOpSAddrImm opcode saCTR immCTR
      -- Parse an instruction that has a short address or an immediate operand.
     = do
      _ <- try $ symbol' opcode
      (saCTR <$> try pShortAddress) <|> (immCTR <$> try pImmediate)

brackets :: Parser a -> Parser a
brackets = symbol "[" `between` symbol "]"

-- | Parse an address operand ("0x2A2A")
pAddressExpr :: Parser Address
pAddressExpr = (Lbl <$> pLabelIdent <|> LitAddr <$> pLitAddr) <?> "address"
  where
    pLitAddr = do
      addr <- brackets pHexadecimal
      if addr <= 0xFFFF
        then return $ fromIntegral addr
        else fail $ printf "address can be at most 0xFFFF (not 0x%02X)" addr

-- | Parse an address in shortened form ("0x2A")
pShortAddress :: Parser ShortAddress
pShortAddress = do
  addr <- brackets pHexadecimal <?> "short address"
  if addr < 0xFF
    then return $ LitShortAddr $ fromIntegral addr
    else fail $ printf "short address can be at most 0xFF (not 0x%02X)" addr

-- | Parse an immediate byte value
pImmediate :: Parser Immediate
pImmediate = Imm . fromIntegral <$> pNumber <?> "immediate value"

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
pLabelIdent :: Parser Text
pLabelIdent =
  lexeme $ do
    p <- fromMaybe "" <$> maybeDot
    p' <- append p <$> (singleton <$> fstLetter)
    append p' . pack <$> many otherLetter <?> "label"
  where
    maybeDot = (try . optional) (singleton <$> char '.')
    fstLetter = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_'
    otherLetter = fstLetter <|> satisfy isDigit
