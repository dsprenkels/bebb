{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AST where

import           RIO

-- AST nodes:
type AST n = [Decl n]

data Decl n = InstrDecl (n (Instruction n))
    | LblDecl (n Label)

data Instruction n = Instr
    { mnemonic :: !(n Text)
    , opnds  :: [n (Operand n)]
    }

data Operand n = OpI !(n Immediate)
    | OpR !(n Register)
    | OpL !(n Label)
    | OpA !(n Address)

newtype Immediate =
  Imm Word8
  deriving (Show, Eq)

newtype Register =
  Reg Text
  deriving (Show, Eq)

newtype Label =
  Lbl Text
  deriving (Show, Eq)

newtype Address =
  Addr Word16
  deriving (Show, Eq)
