{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Emit where

import AST
import Data.Bits
import Error (Error, Error(..), ErrorBundle)
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as NE'
import RIO.State
import qualified TargetDesc as TD
import Text.Printf (printf)

mapFst :: (a -> a) -> (a, b) -> (a, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> b) -> (a, b) -> (a, b)
mapSnd f (a, b) = (a, f b)

-- TODO(dsprenkels) Implement constant definitions
newtype ValueRef =
  LabelRef (WithPos Text)
  deriving (Show)

data Value =
  Value
    { value :: Int
    , ref :: ValueRef
    }
  deriving (Show)

type ConstPool = Map Text Value

data EmitState =
  EmitState
    { offset :: Int
    , maybeGlobalScope :: Maybe Text
    , cp :: ConstPool
     -- TODO(dsprenkels) Optimize construction of `errs`, it is currently O(n^2)
    , errs :: [Error]
    }
  deriving (Show)

instance Semigroup EmitState where
  es1 <> es2 =
    EmitState
      { offset = offset es1 + offset es2
      , maybeGlobalScope = maybeAnd (maybeGlobalScope es2) (maybeGlobalScope es1)
      , cp = cp es1 <> cp es2
      , errs = errs es1 <> errs es2
      }
    where
      maybeAnd Nothing right = right
      maybeAnd left _ = left

instance Monoid EmitState where
  mempty = EmitState {offset = 0, maybeGlobalScope = Nothing, cp = mempty, errs = mempty}

registerLabelsAST :: AST WithPos -> State EmitState ()
registerLabelsAST [] = return mempty
registerLabelsAST (decl:rest) = do
  EmitState {maybeGlobalScope, cp, offset} <- get
  case decl of
    LblDecl textN -> do
      let text = unpackNode textN
      let label = inScope maybeGlobalScope text
      case Map.lookup text cp of
        Just _ -> do
          let msg = printf "symbol '%s' is already defined" label :: String
          let err = CustomError msg (unpackSpan textN)
          modify (<> mempty {errs = [err]})
        Nothing -> do
          let val = Value {value = offset, ref = LabelRef textN}
          let newMGL =
                if isGlobal text
                  then Just text
                  else Nothing
          modify (<> mempty {maybeGlobalScope = newMGL, cp = Map.singleton text val})
    InstrDecl instrN -> moveOffset (emitInstr instrN)
    DataDecl dataN -> moveOffset (emitData dataN)
  registerLabelsAST rest
  where
    moveOffset :: State EmitState a -> State EmitState ()
    moveOffset emitFn = do
      es <- get
      -- Try to build the code, but only register the change in offset
      let EmitState {offset} = execState emitFn es
      put (es {offset})

emit :: AST WithPos -> Either ErrorBundle ByteString
emit ast
  | not $ null $ errs esWithLabels = Left $ NE'.fromList $ errs esWithLabels
  | not $ null $ errs esComplete = Left $ NE'.fromList $ errs esComplete
  | otherwise = Right $ BS.pack binComplete
  where
    esWithLabels = execState (registerLabelsAST ast) mempty
    (binComplete, esComplete) = runState (emitASTCode ast) (reset esWithLabels)
    -- | Throw away bad code information
    reset es = es {offset = 0}

emitASTCode :: AST WithPos -> State EmitState [Word8]
emitASTCode [] = return mempty
emitASTCode (decl:rest) = do
  bin <-
    case decl of
      LblDecl _ -> return []
      InstrDecl instrN -> emitInstr instrN
      DataDecl dataN -> emitData dataN
  bin' <- emitASTCode rest
  return (bin ++ bin')

emitInstr :: WithPos (Instruction WithPos) -> State EmitState [Word8]
emitInstr instrN = do
  let Instr {mnemonic = mnemonicN, opnds} = unpackNode instrN
  let ss = unpackSpan instrN
  let mnemonic = unpackNode mnemonicN
  let mnemonicMatches = filter (\instr -> TD.mnemonic instr == mnemonic) TD.bebbInstrs
  let opndDesc = getOpndDesc opnds
  let isGoodOpndDesc instr = Just (TD.operandDesc instr) == opndDesc
  if null mnemonicMatches
    then do
      let msg = printf "invalid mnemonic ('%s')" mnemonic
      emitError $ CustomError msg (unpackSpan mnemonicN)
      return []
    else case filter isGoodOpndDesc mnemonicMatches of
           [] -> do
             let msg = printf "invalid operands for '%s' instruction (%s)" mnemonic (show opndDesc)
             emitError $ CustomError msg ss
             return []
           [instrDesc] -> do
             bin <- emitOpcode instrDesc
             bin' <- emitOpnds (TD.operandDesc instrDesc) opnds
             return (bin ++ bin')
           more -> do
             let msg =
                   printf
                     "instruction usage is ambiguous; target description is incorrect!\n%s"
                     (show more)
             emitError $ InternalCompilerError msg ss
             return []

emitData :: WithPos (Expr WithPos) -> State EmitState [Word8]
emitData dataN = do
  data_ <- evalExpr (unpackNode dataN)
  if not (0 <= data_ && data_ < 256)
    then do
      let msg = printf "data should be a valid byte value (%d)" data_
      emitError $ CustomError msg (unpackSpan dataN)
      return []
    else emitBinary [fromIntegral data_]

getOpndDesc :: [WithPos (Operand WithPos)] -> Maybe TD.OperandDesc
getOpndDesc [] = Just TD.NoOpnd
getOpndDesc [opndN] =
  let opnd = unpackNode opndN
   in case opnd of
        Imm _ -> Just TD.ImmOpnd
        Addr _ -> Just TD.AddrOpnd
getOpndDesc _ = Nothing

emitOpcode :: TD.Instruction -> State EmitState [Word8]
emitOpcode TD.Instr {TD.opcode = opcode} = emitBinary [opcode]

emitOpnds :: TD.OperandDesc -> [WithPos (Operand WithPos)] -> State EmitState [Word8]
emitOpnds TD.NoOpnd [] = return []
emitOpnds TD.ImmOpnd [opndN]
  -- TODO(dsprenkels) Error on {over,under}flow
 = do
  let expr = unpackOpnd $ unpackNode opndN
  val <- evalExpr expr
  emitBinary [fromIntegral val]
emitOpnds TD.AddrOpnd [opndN]
  -- TODO(dsprenkels) Error on {over,under}flow
 = do
  let expr = unpackOpnd $ unpackNode opndN
  val <- evalExpr expr
  let loByte = fromIntegral val
  let hiByte = fromIntegral (val `shiftR` 8)
  emitBinary [loByte, hiByte]
emitOpnds _ _ = error "unreachable"

emitBinary :: [Word8] -> State EmitState [Word8]
emitBinary bin = do
  modify (<> mempty {offset = length bin})
  return bin

emitError :: Error -> State EmitState ()
emitError err = modify (<> mempty {errs = [err]})

evalExpr :: Expr WithPos -> State EmitState Int
evalExpr (Ident identN) = do
  EmitState {maybeGlobalScope, cp} <- get
  let ident = inScope maybeGlobalScope (unpackNode identN)
  let ss = unpackSpan identN
  case cp Map.!? unpackNode identN of
    Just Value {value} -> return value
    Nothing -> do
      let msg = printf "`%s` is not defined" ident
      emitError $ CustomError msg ss
      return 0
evalExpr (Lit litN) = return (unpackNode litN)
evalExpr (Binary op leftN rightN) = do
  left <- evalExpr $ unpackNode leftN
  right <- evalExpr $ unpackNode rightN
  return $ getBinOp op left right
evalExpr (Unary op opndN) = do
  opnd <- evalExpr $ unpackNode opndN
  return $ getUnOp op opnd

inScope :: Maybe Text -> Text -> Text
inScope Nothing name = name
inScope (Just scope) name
  | (not . isGlobal) name = scope <> name
  | otherwise = name

getBinOp :: BinOp -> (Int -> Int -> Int)
getBinOp Add = (+)
getBinOp Sub = (-)
getBinOp Mul = (*)
getBinOp Div = quot
getBinOp Mod = rem
getBinOp And = (.&.)
getBinOp Or = (.|.)
getBinOp Xor = xor

getUnOp :: UnOp -> (Int -> Int)
getUnOp Pos = id
getUnOp Neg = negate
getUnOp Not = complement
