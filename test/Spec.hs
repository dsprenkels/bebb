{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Main
  ( main
  ) where

import           Asm
import           AST
import           RIO
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec       hiding (parse)

newtype NoPos a =
  NP a
  deriving (Show, Eq)

instance Node NoPos where
  newNode parser = NP <$> parser
  unpackNode (NP x) = x

deriving instance Show (Decl NoPos)

deriving instance Eq (Decl NoPos)

deriving instance Show (Instruction NoPos)

deriving instance Eq (Instruction NoPos)

deriving instance Show (Operand NoPos)

deriving instance Eq (Operand NoPos)

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse p = runParser p "<test input>"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spNumber
  spAddress
  spDecl
  spExample

spNumber :: Spec
spNumber =
  describe "pNumber" $ do
    it "hexadecimal" $ parse pNumber "0x2A" `shouldParse` 42
    it "decimal" $ parse pNumber "42" `shouldParse` 42
    it "negative decimal" $ parse pNumber "-42" `shouldParse` (-42)
    it "binary" $ parse pNumber "0b101010" `shouldParse` 42

spAddress :: Spec
spAddress =
  describe "pAddress" $
  it "literal" $ parse pAddress "[0x2A2A]" `shouldParse` Addr 0x2A2A

spDecl :: Spec
spDecl =
  describe "pDecl" $ do
    it "global label" $
      parse pDecl "_start:\n" `shouldParse` LblDecl (NP $ Lbl "_start")
    it "local label" $
      parse pDecl ".loop1:\n" `shouldParse` LblDecl (NP $ Lbl ".loop1")
    it "add instruction" $
      parse pDecl "  add r3\n" `shouldParse`
      InstrDecl
        (NP $ Instr {mnemonic = NP "add", opnds = [NP $ OpR $ NP $ Reg "r3"]})
    it "globalLabelIdent" $ parse pLabel "_start" `shouldParse` Lbl "_start"
    it "localLabelIdent" $ parse pLabel ".L1" `shouldParse` Lbl ".L1"

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
      \  lda #0\n\
      \  sta r0\n\
      \  lda #1\n\
      \  sta r1\n\
      \\n\
      \  lda #10\n\
      \.loop1:\n\
      \  sta r3 ; loop counter\n\
      \\n\
      \  lda r1\n\
      \  add r0\n\
      \  sta r2\n\
      \  lda r1\n\
      \  sta r0\n\
      \  lda r2\n\
      \  sta r1\n\
      \\n\
      \  lda r3\n\
      \  sub #1\n\
      \  jz .loop1_break\n\
      \  jmp .loop1\n\
      \\n\
      \.loop1_break:\n\
      \  ret\n\
      \\n"
