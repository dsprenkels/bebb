{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module SyntaxSpec
  ( spec
  ) where

import AST
import RIO
import Syntax hiding (parse)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec hiding (parse)

newtype NoPos a =
  NP a
  deriving (Show, Eq)

instance Node NoPos where
  newNode node (_, _) = NP node
  nodeFrom _ = NP
  unpackNode (NP x) = x
  unpackSpan _ = (0, 0)

deriving instance Show (Decl NoPos)

deriving instance Eq (Decl NoPos)

deriving instance Show (Instruction NoPos)

deriving instance Eq (Instruction NoPos)

deriving instance Show (Operand NoPos)

deriving instance Eq (Operand NoPos)

deriving instance Show (Expr NoPos)

deriving instance Eq (Expr NoPos)

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse p = runParser p "<test input>"

spec :: Spec
spec = do
  pNumberSpec
  pLineSpec
  pExprSpec
  pExampleSpec
  pIssue1Spec

pNumberSpec :: Spec
pNumberSpec =
  describe "pNumber" $ do
    it "hexadecimal" $ parse pNumber "0x2A" `shouldParse` 42
    it "decimal" $ parse pNumber "42" `shouldParse` 42
    it "negative decimal" $ parse pNumber "-42" `shouldParse` (-42)
    it "binary" $ parse pNumber "0b101010" `shouldParse` 42

pLineSpec :: Spec
pLineSpec =
  describe "pLine" $ do
    it "global label" $ shouldParse (parse pLine "_start:\n") [LblDecl (NP "_start")]
    it "local label" $ shouldParse (parse pLine ".loop1:\n") [LblDecl (NP ".loop1")]
    it "add instruction" $
      shouldParse
        (parse pLine "add r3\n")
        [InstrDecl (NP Instr {mnemonic = NP "add", opnds = [NP $ Addr $ Ident $ NP "r3"]})]
    it "globalLabelIdent" $ shouldParse (parse pLabel "_start") "_start"
    it "localLabelIdent" $ shouldParse (parse pLabel ".L1") ".L1"
    it "data" $ shouldParse (parse pLine "1, 2, 3\n") (DataDecl . NP . Lit . NP <$> [1, 2, 3])

pExprSpec :: Spec
pExprSpec =
  describe "pExpr" $
  -- Terminators
   do
    it "decimal" $ parse pExpr "3" `shouldParse` Lit (NP 3)
    it "hex" $ parse pExpr "0x3" `shouldParse` Lit (NP 3)
    it "identifier" $ parse pExpr "_start" `shouldParse` Ident (NP "_start")
    it "identifier with dot" $ parse pExpr ".L1" `shouldParse` Ident (NP ".L1")
    it "parentheses" $ parse pExpr "(3)" `shouldParse` Lit (NP 3)
  -- Composite expressions
    it "add (normal)" $
      parse pExpr "1 + 2 + 3" `shouldParse`
      Binary Add (NP (Binary Add (NP $ Lit $ NP 1) (NP $ Lit $ NP 2))) (NP $ Lit $ NP 3)
    it "add (right)" $
      parse pExpr "1 + (2 + 3)" `shouldParse`
      Binary Add (NP $ Lit $ NP 1) (NP (Binary Add (NP $ Lit $ NP 2) (NP $ Lit $ NP 3)))
    it "add & mul (left)" $
      parse pExpr "1 * 2 + 3" `shouldParse`
      Binary Add (NP (Binary Mul (NP $ Lit $ NP 1) (NP $ Lit $ NP 2))) (NP $ Lit $ NP 3)
    it "add & mul (right)" $
      parse pExpr "1 + 2 * 3" `shouldParse`
      Binary Add (NP $ Lit $ NP 1) (NP (Binary Mul (NP $ Lit $ NP 2) (NP $ Lit $ NP 3)))
    it "unary" $ parse pExpr "-3" `shouldParse` Unary Neg (NP $ Lit $ NP 3)

pExampleSpec :: Spec
pExampleSpec = describe "example" $ it "fibonacci" $ parse p `shouldSucceedOn` asm
  where
    p :: Parser (AST NoPos)
    p = pASM
    asm =
      "\
      \forty_two: 42\
      \\n\
      \fibonacci:\n\
      \  ; Compute 10 steps of the fibonacci sequence.\n\
      \\n\
      \  lda #0 ; mark\n\
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

pIssue1Spec :: Spec
pIssue1Spec = describe "issue #1" $ do
     it "empty file" $ parse p `shouldSucceedOn` ""
     it "no newline after label" $ parse p `shouldSucceedOn` "_start:"
     it "no newline after instruction" $ parse p `shouldSucceedOn` "lda 0"
     it "no newline after data" $ parse p `shouldSucceedOn` "0"
  where
    p :: Parser (AST NoPos)
    p = pASM
