-- !! https://haskell.godbolt.org/z/-5mTew
-- TODO(dsprenkels): Add a custom error reporting function, that not only
-- supports parse errors, but also type errors etc.
--
-- TODO(dsprenkels): Do not "parse" instructions and opcodes immediately, but
-- parse instructions as-is, and later check if they are correct.
--
-- TODO(dsprenkels): Put AST definition in AST.hs
--
module Asm where

import           RIO                        hiding (many, some, try)
import           RIO.Char                   (isAlpha, isAlphaNum, isAsciiLower,
                                             isAsciiUpper, isDigit)
import           RIO.Text                   (append, pack, singleton)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf                (printf)

data SourceSpan =
  SourceSpan
    { lo :: SourcePos
    , hi :: SourcePos
    }
  deriving (Show, Eq)

data WithPos a =
  WP
    { a  :: a
    , ss :: SourceSpan
    }
  deriving (Show, Eq)

class Node (n :: * -> *) where
  newNode :: Parser a -> Parser (n a)
  unpackNode :: n a -> a

instance Node WithPos where
  newNode parser = do
    lo <- getSourcePos
    a <- parser
    hi <- getSourcePos
    return WP {a, ss = SourceSpan {lo, hi}}
  unpackNode WP {a} = a

-- AST nodes:
type AST n = [Decl n]

data Decl n
  = InstrDecl (n (Instruction n))
  | LblDecl (n Address)

data Instruction n =
  Instr
    { opcode :: n Text
    , args   :: [n (Argument n)]
    }

data Argument n
  = ArgImm (n Immediate)
  | ArgSA (n ShortAddress)
  | ArgA (n Address)

newtype Immediate =
  Imm Word8
  deriving (Show, Eq)

newtype ShortAddress =
  LitShortAddr Word8
  deriving (Show, Eq)

data Address
  = Lbl Text
  | LitAddr Word16
  deriving (Show, Eq)

data Register
  = PC
  | SP
  | R Int
  deriving (Show, Eq)

deriving instance Show (Decl WithPos)

deriving instance Eq (Decl WithPos)

deriving instance Show (Instruction WithPos)

deriving instance Eq (Instruction WithPos)

deriving instance Show (Argument WithPos)

deriving instance Eq (Argument WithPos)

-- | Our custom Megaparsec parser type
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

-- | Parse a DM assmebly source file
pASM :: (Node a) => Parser (AST a)
pASM = concat <$> many pLine <* eof

-- | Parse a line
pLine :: (Node a) => Parser [Decl a]
pLine = (maybeToList <$> optional pDecl) <* sc <* char '\n'

-- | Parse a general declaration
pDecl :: (Node a) => Parser (Decl a)
pDecl = try pLabelDecl <|> try pInstructionDecl

-- | Parse a label declaration
pLabelDecl :: (Node a) => Parser (Decl a)
pLabelDecl = (LblDecl <$> newNode pAddressExpr) <* symbol ":"

-- | Parse an instruction declaration (i.e. a line containing an instruction)
pInstructionDecl :: (Node a) => Parser (Decl a)
pInstructionDecl = do
  void $ some (char ' ' <|> char '\t') -- Require indentation
  InstrDecl <$> newNode pInstruction

-- | Parse an instruction
pInstruction :: (Node a) => Parser (Instruction a)
pInstruction = do
  opcode <- newNode $ lexeme pOpcode
  args <- many $ newNode ((pImmArg <|> pSAArg <|> pAArg) <?> "instruction argument")
  return $ Instr {opcode, args}
  where
    pImmArg = ArgImm <$> newNode pImmediate
    pSAArg = ArgSA <$> newNode pShortAddress
    pAArg = ArgA <$> newNode pAddressExpr

pOpcode :: Parser Text
pOpcode = do
  c <- satisfy isAlpha
  rest <- many $ satisfy isAlphaNum
  return $ pack $ c : rest

-- | Wrap a parser between brackets ("[ ... ]")
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
pImmediate = (Imm . fromIntegral <$> pNumber) <?> "immediate value"

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
