{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}

module AST where

import RIO

-- Location tracking
type Span = (Int, Int)

class Node (n :: * -> *) where
  newNode :: a -> (Int, Int) -> n a
  nodeFrom :: (n b, n c) -> a -> n a
  unpackNode :: n a -> a
  unpackSpan :: n a -> Span

data WithPos a =
  WP
    { inner :: a
    , ss :: Span
    }
  deriving (Show, Eq)

instance Node WithPos where
  newNode node (lo, hi) = WP {inner = node, ss = (lo, hi)}
  nodeFrom (n1, n2) node = WP {inner = node, ss = (lo, hi)}
    where
      WP {ss = (lo, _)} = n1
      WP {ss = (_, hi)} = n2
  unpackNode WP {inner} = inner
  unpackSpan WP {ss} = ss

deriving instance Show (Decl WithPos)

deriving instance Eq (Decl WithPos)

deriving instance Show (Instruction WithPos)

deriving instance Eq (Instruction WithPos)

deriving instance Show (Operand WithPos)

deriving instance Eq (Operand WithPos)

deriving instance Show (Expr WithPos)

deriving instance Eq (Expr WithPos)

-- AST nodes:
type AST n = [Decl n]

data Decl n
  = LblDecl (n Label)
  | InstrDecl (n (Instruction n))
  | DataDecl (n (Expr n))

data Instruction n =
  Instr
    { mnemonic :: !(n Text)
    , opnds :: [n (Operand n)]
    }

data Operand n
  = Imm (Expr n)
  | Addr (Expr n)

data Expr n
  = Ident !(n Identifier)
  | Lit !(n Literal)
  | Binary !BinOp !(n (Expr n)) !(n (Expr n))
  | Unary !UnOp !(n (Expr n))

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Xor
  deriving (Show, Eq)

type Identifier = Text

type Literal = Int

data UnOp
  = Pos
  | Neg
  | Not
  deriving (Show, Eq)

type Label = Text
