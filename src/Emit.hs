{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Emit where

import AST
import Error (Error, Error(..), ErrorBundle)
import RIO
import qualified RIO.Map as Map
import RIO.State
import qualified TargetDesc as TD
import Text.Printf (printf)

type ConstPool = Map Text Int

data EmitState =
  EmitState
    { offset :: Int
    , bin :: [Word8]
    , cp :: ConstPool
    , errs :: [Error]
    }
  deriving (Show)

instance Semigroup EmitState where
  es1 <> es2 =
    EmitState
      { offset = offset es2
      , bin = bin es1 <> bin es2
      , cp = cp es1 <> cp es2
      , errs = errs es1 <> errs es2
      }

instance Monoid EmitState where
  mempty = EmitState {offset = 0, bin = mempty, cp = mempty, errs = mempty}

emit :: Node a => AST a -> EmitState
emit ast = execState (emitASTCode ast) mempty

emitASTCode :: Node a => AST a -> State EmitState ()
emitASTCode [] = return mempty
emitASTCode (decl:rest) = do
  EmitState {offset, bin, cp, errs} <- get
  case decl of
    LblDecl node -> do
      let text = unpackNode node
      case text `Map.lookup` cp of
        Just loc -> do
          let msg = printf "symbol '%s' is already defined" text :: String
          let err = CustomError msg (unpackSpan node)
          modify (<> mempty {errs = [err]})
        Nothing -> modify (<> mempty {cp = Map.singleton text offset})
    InstrDecl node -> return ()
    DataDecl node -> return ()
  emitASTCode rest

emitInstr :: Node a => Instruction a -> Span -> State EmitState ()
emitInstr Instr {mnemonic = mnemonicN, opnds} span = do
  EmitState {offset, bin, cp, errs} <- get
  let mnemonic = unpackNode mnemonicN
  let mnemonicMatches = filter (\instr -> TD.mnemonic instr == mnemonic) TD.bebbInstrs
  let opndDesc = getOpndDesc opnds
  let isGoodOpndDesc instr = Just (TD.operandDesc instr) == opndDesc
  if null mnemonicMatches
    then do
      let msg = printf "invalid mnemonic ('%s')" mnemonic
      let err = CustomError msg (unpackSpan mnemonicN)
      modify (<> mempty {errs = [err]})
    else case filter isGoodOpndDesc mnemonicMatches of
           [] -> do
             let msg = printf "invalid operands for '%s' instruction (%s)" mnemonic (show opndDesc)
             let err = CustomError msg span
             modify (<> mempty {errs = [err]})
           [instrDesc] -> do
             emitOpcode instrDesc
             let _ = error "unimplemented"
             return ()
           _ -> do
             let msg = "instruction usage is ambiguous; target description is incorrect!"
             let err = InternalCompilerError msg span
             modify (<> mempty {errs = [err]})

getOpndDesc :: Node a => [a (Operand a)] -> Maybe TD.OperandDesc
getOpndDesc [] = Just TD.NoOpnd
getOpndDesc [opndN] =
  let opnd = unpackNode opndN
   in case opnd of
        Imm _ -> Just TD.ImmOpnd
        Addr _ -> Just TD.AddrOpnd
        _ -> Nothing

emitOpcode :: TD.Instruction -> State EmitState ()
emitOpcode TD.Instr {TD.opcode = opcode} = do
  EmitState {offset, bin} <- get
  put EmitState {offset = offset + 1, bin = bin ++ [opcode]}
