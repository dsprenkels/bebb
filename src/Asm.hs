{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm where

import           RIO hiding (try, many, some)
import           RIO.Char (isAsciiLower, isAsciiUpper, isDigit)
import           RIO.Text (singleton, append, pack)
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Char

data Register = PC
              | SP
  deriving (Show, Eq)

data Span = Span { lo :: SourcePos, hi :: SourcePos }
  deriving (Show, Eq)

data Address = Lbl Label
             | LitAddr Word16
  deriving (Show, Eq)

newtype ShortAddress = LitShortAddr Word8
  deriving (Show, Eq)

newtype Immediate = Imm Word8
  deriving (Show, Eq)

type Label = Text

data Instruction =
    LDA Address
  | STA Address
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
  deriving (Show, Eq)

data Decl = InstrDecl Instruction
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
lexeme = L.lexeme scn

-- | Parse a DM assmebly source file
pASM :: Parser AST
pASM = error "unimplemented"

-- | Parse a general declaration
pDecl :: Parser Decl
pDecl = pLabelDecl <|> pInstructionDecl

-- | Parse a label declaration
pLabelDecl :: Parser Decl
pLabelDecl = (LblDecl <$> pAddressExpr) <* L.symbol sc ":" <* char '\n'

-- | Parse an instruction declaration (i.e. a line containing an instruction)
pInstructionDecl :: Parser Decl
pInstructionDecl = do
  _ <- some (char ' ' <|> char '\t') -- Require indentation
  (InstrDecl <$> pInstruction) <* char '\n'

-- | Parse an instruction
pInstruction :: Parser Instruction
pInstruction = choice
  [ pOpAddr "LDA" LDA
  , pOpAddr "STA" STA
  , pOpSAddr "ADD" ADD
  , pOpImm "ADDI" ADDI
  , pOpSAddr "ADC" ADC
  , pOpImm "ADCI" ADCI
  , pOpSAddr "SUB" SUB
  , pOpImm "SUBI" SUBI
  , pOpSAddr "SUBC" SUBC
  , pOpImm "SUBCI" SUBCI
  , pOpSAddr "OR" OR
  , pOpImm "ORI" ORI
  , pOpSAddr "AND" AND
  , pOpImm "ANDI" ANDI
  , pOpSAddr "XOR" XOR
  , pOpImm "XORI" XORI]
  <?> "instruction"
  where
    pOpAddr :: Text -> (Address -> Instruction) -> Parser Instruction
    pOpAddr opcode ctr = try $ ctr <$> (L.symbol' sc opcode *> pAddressExpr)

    pOpSAddr :: Text -> (ShortAddress -> Instruction) -> Parser Instruction
    pOpSAddr opcode ctr = try $ ctr <$> (L.symbol' sc opcode *> pShortAddress)

    pOpImm :: Text -> (Immediate -> Instruction) -> Parser Instruction
    pOpImm opcode ctr = try $ ctr <$> (L.symbol' sc opcode *> pImmediate)

-- | Parse an address operand ("0x2A2A")
pAddressExpr :: Parser Address
pAddressExpr = (Lbl <$> pLabelIdent <|> LitAddr . fromIntegral <$> pHexadecimal)
  <?> "address"

-- | Parse an address in shortened form ("0x2A")
pShortAddress :: Parser ShortAddress
pShortAddress =
  LitShortAddr . fromIntegral <$> pHexadecimal <?> "short address"

-- | Parse an immediate byte value
pImmediate :: Parser Immediate
pImmediate = Imm . fromIntegral <$> pNumber <?> "immediate value"

-- | Parse a number
pNumber :: Parser Int
pNumber = pHexadecimal <|> pBinary <|> pDecimal

-- | Parse a decimal value ("42")
pDecimal :: Parser Int
pDecimal = L.lexeme sc (L.signed sc L.decimal) <?> "decimal value"

-- | Parse a hexadecimal value ("0x2A")
pHexadecimal :: Parser Int
pHexadecimal = L.lexeme sc (string' "0x" *> L.hexadecimal) <?> "hex value"

-- | Parse a binary value ("0b101010")
pBinary :: Parser Int
pBinary = L.lexeme sc (string' "0b" *> L.binary) <?> "binary value"

-- | Parse a label identifier ("_start", ".loop1", etc.)
pLabelIdent :: Parser Text
pLabelIdent = lexeme
  $ do
    p <- fromMaybe "" <$> maybeDot
    p' <- append p <$> (singleton <$> fstLetter)
    append p' . pack <$> many otherLetter <?> "label"
  where
    maybeDot = (try . optional) (singleton <$> char '.')

    fstLetter = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_'

    otherLetter = fstLetter <|> satisfy isDigit

