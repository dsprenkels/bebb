{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Asm
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec hiding (parse)

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
  spExampleAssembly

spNumber :: Spec
spNumber =
  describe "pNumber" $ do
    it "hexadecimal" $ parse pNumber "0x2A" `shouldParse` 42
    it "decimal" $ parse pNumber "42" `shouldParse` 42
    it "negative decimal" $ parse pNumber "-42" `shouldParse` (-42)
    it "binary" $ parse pNumber "0b101010" `shouldParse` 42

spAddressExpr :: Spec
spAddressExpr =
  describe "pAddressExpr" $ do
    it "literal" $ parse pAddressExpr "[0x1337]" `shouldParse` LitAddr 0x1337
    it "globalLabelIdent" $
      parse pAddressExpr "_start" `shouldParse` Lbl "_start"
    it "localLabelIdent" $ parse pAddressExpr ".L1" `shouldParse` Lbl ".L1"

spInstruction :: Spec
spInstruction =
  describe "pInstruction" $ do
    it "lda" $
      parse pInstruction "lda [0x2A2A]" `shouldParse` LDA (LitAddr 0x2A2A)
    it "lda w/ short address" $
      parse pInstruction "lda [0x2A]" `shouldParse` LDAB (LitShortAddr 0x2A)
    it "lda" $
      parse pInstruction "lda [0x2A2A]" `shouldParse` LDA (LitAddr 0x2A2A)
    it "sta" $
      parse pInstruction "sta [0x2A2A]" `shouldParse` STA (LitAddr 0x2A2A)
    it "add w/ short address" $
      parse pInstruction "add [0x2A]" `shouldParse` ADD (LitShortAddr 0x2A)
    it "add w/ immediate" $
      parse pInstruction "add 0x2A" `shouldParse` ADDI (Imm 0x2A)
    it "adc w/ short address" $
      parse pInstruction "adc [0x2A]" `shouldParse` ADC (LitShortAddr 0x2A)
    it "adc w/ immediate" $
      parse pInstruction "adc 0x2A" `shouldParse` ADCI (Imm 0x2A)
    it "sub w/ short address" $
      parse pInstruction "sub [0x2A]" `shouldParse` SUB (LitShortAddr 0x2A)
    it "sub w/ immediate" $
      parse pInstruction "sub 0x2A" `shouldParse` SUBI (Imm 0x2A)
    it "subc w/ short address" $
      parse pInstruction "subc [0x2A]" `shouldParse` SUBC (LitShortAddr 0x2A)
    it "subc w/ immediate" $
      parse pInstruction "subc 0x2A" `shouldParse` SUBCI (Imm 0x2A)
    it "or w/ short address" $
      parse pInstruction "or [0x2A]" `shouldParse` OR (LitShortAddr 0x2A)
    it "or w/ immediate" $
      parse pInstruction "or 0x2A" `shouldParse` ORI (Imm 0x2A)
    it "and w/ short address" $
      parse pInstruction "and [0x2A]" `shouldParse` AND (LitShortAddr 0x2A)
    it "and w/ immediate" $
      parse pInstruction "and 0x2A" `shouldParse` ANDI (Imm 0x2A)
    it "xor w/ short address" $
      parse pInstruction "xor [0x2A]" `shouldParse` XOR (LitShortAddr 0x2A)
    it "xor w/ immediate" $
      parse pInstruction "xor 0x2A" `shouldParse` XORI (Imm 0x2A)

spDecl :: Spec
spDecl =
  describe "pDecl" $ do
    it "global label" $
      parse pDecl "_start:\n" `shouldParse` LblDecl (Lbl "_start")
    it "local label" $
      parse pDecl ".loop1:\n" `shouldParse` LblDecl (Lbl ".loop1")
    it "literal address label" $
      parse pDecl "[0x2A2A]:\n" `shouldParse` LblDecl (LitAddr 0x2A2A)
    it "space before global label" $ parse pDecl `shouldFailOn` " _start:\n"
    it "space before local label" $ parse pDecl `shouldFailOn` " .loop1:\n"
    it "add instruction" $
      parse pDecl "  add [0x2A]\n" `shouldParse`
      InstrDecl (ADD (LitShortAddr 0x2A))

spExampleAssembly :: Spec
spExampleAssembly =
  describe "example1" $ it "parse" $ parse pASM asm `shouldParse` expected
  where
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
    expected =
      [ LblDecl (Lbl "fibonacci")
      , InstrDecl (LDAI (Imm 0))
      , InstrDecl (STAB (LitShortAddr 0))
      , InstrDecl (LDAI (Imm 1))
      , InstrDecl (STAB (LitShortAddr 1))
      , InstrDecl (LDAI (Imm 10))
      , LblDecl (Lbl ".loop1")
      , InstrDecl (STAB (LitShortAddr 3))
      , InstrDecl (LDAB (LitShortAddr 1))
      , InstrDecl (ADD (LitShortAddr 0))
      , InstrDecl (STAB (LitShortAddr 2))
      , InstrDecl (LDAB (LitShortAddr 1))
      , InstrDecl (STAB (LitShortAddr 0))
      , InstrDecl (LDAB (LitShortAddr 2))
      , InstrDecl (STAB (LitShortAddr 1))
      , InstrDecl (LDAB (LitShortAddr 3))
      , InstrDecl (SUBI (Imm 1))
      , InstrDecl (JZ (Lbl ".loop1_break"))
      , InstrDecl (JMP (Lbl ".loop1"))
      , LblDecl (Lbl ".loop1_break")
      , InstrDecl RET
      ]
