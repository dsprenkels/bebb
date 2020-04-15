{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}

module AST where

import RIO
import qualified RIO.Text as T

-- Location tracking
type Span = (Int, Int)

class Node (n :: * -> *) where
  newNode :: a -> Span -> n a
  nodeFrom :: (n b, n c) -> a -> n a
  nodeMap :: (a -> b) -> n a -> n b
  nodeMap f node =
    newNode (f $ unpackNode node) (unpackSpan node)
  unpackNode :: n a -> a
  unpackSpan :: n a -> Span

data WithPos a =
  WP
    { nodeInner :: a
    , ss :: Span
    }
  deriving (Show, Eq)

instance Node WithPos where
  newNode node (lo, hi) = WP {nodeInner = node, ss = (lo, hi)}
  nodeFrom (n1, n2) node = WP {nodeInner = node, ss = (lo, hi)}
    where
      WP {ss = (lo, _)} = n1
      WP {ss = (_, hi)} = n2
  unpackNode WP {nodeInner} = nodeInner
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

unpackOpnd :: Operand a -> Expr a
unpackOpnd (Imm expr) = expr
unpackOpnd (Addr expr) = expr

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

isGlobal :: Label -> Bool
isGlobal = not . isLocal

isLocal :: Label -> Bool
isLocal lbl = (fst <$> T.uncons lbl) == Just '.'
