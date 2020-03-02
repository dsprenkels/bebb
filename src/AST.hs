{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AST where

import           RIO

-- AST nodes:
type AST n = [Decl n]

data Decl n = LblDecl (n Label)
    | InstrDecl (n (Instruction n))
    | DataDecl (n (Expr n))

data Instruction n = Instr
    { mnemonic :: !(n Text)
    , opnds    :: [n (Operand n)]
    }

data Operand n = Imm (Expr n)
    | Addr (Expr n)

data Expr n = Ident !(n Identifier)
    | Lit !(n Literal)
    | Binary !BinOp !(n (Expr n)) !(n (Expr n))
    | Unary !UnOp !(n (Expr n))

data BinOp = Add
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

data UnOp = Pos
    | Neg
    | Not
    deriving (Show, Eq)

type Label = Text
