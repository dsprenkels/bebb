{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import           Asm
import           RIO
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec       hiding (parse)

newtype NoPos a =
  NP a
  deriving (Show, Eq)

instance Node NoPos where
  newNode parser = NP <$> parser
  unpackNode (NP a) = a

deriving instance Show (Decl NoPos)

deriving instance Eq (Decl NoPos)

deriving instance Show (Instruction NoPos)

deriving instance Eq (Instruction NoPos)

deriving instance Show (Argument NoPos)

deriving instance Eq (Argument NoPos)

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse p = runParser p "<test input>"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spNumber
  spAddressExpr
  spDecl
  spExample

spNumber :: Spec
spNumber =
  describe "pNumber" $ do
    it "hexadecimal" $ parse pNumber "0x2A" `shouldParse` 42
    it "decimal" $ parse pNumber "42" `shouldParse` 42
    it "negative decimal" $ parse pNumber "-42" `shouldParse` (-42)
    it "binary" $ parse pNumber "0b101010" `shouldParse` 42

spAddressExpr :: Spec
spAddressExpr =
  describe "pAddressExpr" $
  it "literal" $ parse pAddressExpr "[0x2A2A]" `shouldParse` LitAddr 0x2A2A

spDecl :: Spec
spDecl =
  describe "pDecl" $ do
    it "global label" $
      parse pDecl "_start:\n" `shouldParse` LblDecl (NP $ Lbl "_start")
    it "local label" $
      parse pDecl ".loop1:\n" `shouldParse` LblDecl (NP $ Lbl ".loop1")
    it "literal address label" $
      parse pDecl "[0x2A2A]:\n" `shouldParse` LblDecl (NP $ LitAddr 0x2A2A)
    it "add instruction" $
      parse pDecl "  add [0x2A]\n" `shouldParse`
      InstrDecl
        (NP $
         Instr {opcode = NP "add", args = [NP $ ArgSA $ NP $ LitShortAddr 0x2A]})
    it "globalLabelIdent" $
      parse pAddressExpr "_start" `shouldParse` Lbl "_start"
    it "localLabelIdent" $ parse pAddressExpr ".L1" `shouldParse` Lbl ".L1"

spExample :: Spec
spExample = describe "example" $ it "fibonacci" $ parse p `shouldSucceedOn` asm
  where
    p :: Parser (AST NoPos)
    p = pASM
    asm =
      "\
      \fibonacci:\n\
      \  ; Compute 10 steps of the fibonacci sequence.\n\
      \\n\
      \  lda 0\n\
      \  sta [0x00]\n\
      \  lda 1\n\
      \  sta [0x01]\n\
      \\n\
      \  lda 10\n\
      \.loop1:\n\
      \  sta [0x03] ; loop counter\n\
      \\n\
      \  lda [0x01]\n\
      \  add [0x00]\n\
      \  sta [0x02]\n\
      \  lda [0x01]\n\
      \  sta [0x00]\n\
      \  lda [0x02]\n\
      \  sta [0x01]\n\
      \\n\
      \  lda [0x03]\n\
      \  sub 1\n\
      \  jz .loop1_break\n\
      \  jmp .loop1\n\
      \\n\
      \.loop1_break:\n\
      \  ret\n\
      \\n"
