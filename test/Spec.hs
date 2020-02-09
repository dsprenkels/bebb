{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Asm
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec hiding (parse)

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse p = runParser p "<test input>"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spNumber
  spAddressExpr
  spInstruction
  spDecl

spNumber :: Spec
spNumber = describe "pNumber"
  $ do
    it "hexadecimal" $ parse pNumber "0x2A" `shouldParse` 42
    it "decimal" $ parse pNumber "42" `shouldParse` 42
    it "negative decimal" $ parse pNumber "-42" `shouldParse` (-42)
    it "binary" $ parse pNumber "0b101010" `shouldParse` 42

spAddressExpr :: Spec
spAddressExpr = describe "pAddressExpr"
  $ do
    it "literal" $ parse pAddressExpr "0x1337" `shouldParse` LitAddr 0x1337
    it "globalLabelIdent"
      $ parse pAddressExpr "_start" `shouldParse` Lbl "_start"
    it "localLabelIdent" $ parse pAddressExpr ".L1" `shouldParse` Lbl ".L1"

spInstruction :: Spec
spInstruction = describe "pInstruction"
  $ do
    it "lda"
      $ parse pInstruction "lda 0x1337" `shouldParse` LDA (LitAddr 0x1337)
    it "sta"
      $ parse pInstruction "sta 0x1337" `shouldParse` STA (LitAddr 0x1337)
    it "add"
      $ parse pInstruction "add 0x2A" `shouldParse` ADD (LitShortAddr 0x2A)
    it "addi" $ parse pInstruction "addi 0x2A" `shouldParse` ADDI (Imm 0x2A)
    it "adc"
      $ parse pInstruction "adc 0x2A" `shouldParse` ADC (LitShortAddr 0x2A)
    it "adci" $ parse pInstruction "adci 0x2A" `shouldParse` ADCI (Imm 0x2A)
    it "sub"
      $ parse pInstruction "sub 0x2A" `shouldParse` SUB (LitShortAddr 0x2A)
    it "subi" $ parse pInstruction "subi 0x2A" `shouldParse` SUBI (Imm 0x2A)
    it "subc"
      $ parse pInstruction "subc 0x2A" `shouldParse` SUBC (LitShortAddr 0x2A)
    it "subci" $ parse pInstruction "subci 0x2A" `shouldParse` SUBCI (Imm 0x2A)
    it "or" $ parse pInstruction "or 0x2A" `shouldParse` OR (LitShortAddr 0x2A)
    it "ori" $ parse pInstruction "ori 0x2A" `shouldParse` ORI (Imm 0x2A)
    it "and"
      $ parse pInstruction "and 0x2A" `shouldParse` AND (LitShortAddr 0x2A)
    it "andi" $ parse pInstruction "andi 0x2A" `shouldParse` ANDI (Imm 0x2A)
    it "xor"
      $ parse pInstruction "xor 0x2A" `shouldParse` XOR (LitShortAddr 0x2A)
    it "xori" $ parse pInstruction "xori 0x2A" `shouldParse` XORI (Imm 0x2A)

spDecl :: Spec
spDecl = describe "pDecl"
  $ do
    it "global label"
      $ parse pDecl "_start:\n" `shouldParse` LblDecl (Lbl "_start")
    it "local label"
      $ parse pDecl ".loop1:\n" `shouldParse` LblDecl (Lbl ".loop1")
    it "literal address label"
      $ parse pDecl "0x2A2A:\n" `shouldParse` LblDecl (LitAddr 0x2A2A)
    it "space before global label" $ parse pDecl `shouldFailOn` " _start:\n"
    it "space before local label" $ parse pDecl `shouldFailOn` " .loop1:\n"
