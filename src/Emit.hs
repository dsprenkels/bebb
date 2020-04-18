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
    { value :: Int64
    , ref :: ValueRef
    }
  deriving (Show)

type ConstPool = Map Text Value

data EmitState =
  EmitState
    { offset :: Int
    , maybeGlobalScope :: Maybe Text
    , cp :: ConstPool
    }
  deriving (Show)

instance Semigroup EmitState where
  es1 <> es2 =
    EmitState
      { offset = offset es1 + offset es2
      , maybeGlobalScope = maybeAnd (maybeGlobalScope es2) (maybeGlobalScope es1)
      , cp = cp es1 <> cp es2
      }
    where
      maybeAnd Nothing right = right
      maybeAnd left _ = left

instance Monoid EmitState where
  mempty = EmitState {offset = 0, maybeGlobalScope = Nothing, cp = mempty}

data EmitResult a
  = EmitOk a
  | EmitErr (NonEmpty Error)
  deriving (Show)

instance Semigroup a => Semigroup (EmitResult a) where
  EmitErr errs1 <> EmitErr errs2 = EmitErr (errs1 <> errs2)
  EmitErr errs <> EmitOk _ = EmitErr errs
  EmitOk _ <> EmitErr errs = EmitErr errs
  EmitOk ok1 <> EmitOk ok2 = EmitOk (ok1 <> ok2)

instance Monoid a => Monoid (EmitResult a) where
  mempty = EmitOk mempty

instance Functor EmitResult where
  fmap f (EmitOk ok) = EmitOk (f ok)
  fmap _ (EmitErr errs) = EmitErr errs

instance Applicative EmitResult where
  pure = EmitOk
  EmitErr errs1 <*> EmitErr errs2 = EmitErr (errs1 <> errs2)
  EmitErr errs1 <*> EmitOk _ = EmitErr errs1
  EmitOk f <*> r = f <$> r

instance Monad EmitResult where
  (EmitOk ok) >>= f = f ok
  (EmitErr errs) >>= _ = EmitErr errs

data Pass
  = RegisterLabelsPass
  | EmitCodePass
  deriving (Show)

emit :: AST WithPos -> Either ErrorBundle ByteString
emit ast =
  case (erWithLabels, erComplete) of
    (EmitErr errs, _) -> Left errs
    (EmitOk (), EmitErr errs) -> Left errs
    (EmitOk (), EmitOk bin) -> Right $ BS.pack bin
  where
    (erWithLabels, esWithLabels) = runState (registerLabelsAST ast) mempty
    erComplete = evalState (emitASTCode ast) (reset esWithLabels)
    -- | Throw away bad code information
    reset es = es {offset = 0}

registerLabelsAST :: AST WithPos -> State EmitState (EmitResult ())
registerLabelsAST [] = return mempty
registerLabelsAST (decl:rest) = do
  EmitState {maybeGlobalScope, cp, offset} <- get
  result <-
    case decl of
      LblDecl textN -> do
        let text = unpackNode textN
        let label = inScope maybeGlobalScope text
        case Map.lookup text cp of
          Just _ -> do
            let msg = printf "symbol '%s' is already defined" label :: String
            return $ emitError $ CustomError msg (unpackSpan textN)
          Nothing -> do
            let val = Value {value = fromIntegral offset, ref = LabelRef textN}
            let newMGL =
                  if isGlobal text
                    then Just text
                    else Nothing
            modify (<> mempty {maybeGlobalScope = newMGL, cp = Map.singleton text val})
            return $ EmitOk ()
      InstrDecl instrN -> moveOffset (emitInstr RegisterLabelsPass instrN)
      DataDecl dataSizeN dataN -> moveOffset (emitDataGroup RegisterLabelsPass dataSizeN dataN)
  result' <- registerLabelsAST rest
  return (result <> result')
  where
    moveOffset :: State EmitState (EmitResult a) -> State EmitState (EmitResult ())
    moveOffset emitFn = do
      es <- get
      -- Try to build the code, but only register the change in offset
      let (er, EmitState {offset}) = runState emitFn es
      put (es {offset})
      return (() <$ er)

emitASTCode :: AST WithPos -> State EmitState (EmitResult [Word8])
emitASTCode [] = return mempty
emitASTCode (decl:rest) = do
  er <-
    case decl of
      LblDecl _ -> return mempty
      InstrDecl instrN -> emitInstr EmitCodePass instrN
      DataDecl dataSizeN dataN -> emitDataGroup EmitCodePass dataSizeN dataN
  er' <- emitASTCode rest
  return (er <> er')

emitInstr :: Pass -> WithPos (Instruction WithPos) -> State EmitState (EmitResult [Word8])
emitInstr pass instrN = do
  let Instr {mnemonic = mnemonicN, opnds} = unpackNode instrN
  let ss = unpackSpan instrN
  let mnemonic = unpackNode mnemonicN
  let mnemonicMatches = filter (\instr -> TD.mnemonic instr == mnemonic) TD.bebbInstrs
  let opndDesc = getOpndDesc opnds
  let isGoodOpndDesc instr = Just (TD.operandDesc instr) == opndDesc
  if null mnemonicMatches
    then do
      let msg = printf "invalid mnemonic ('%s')" mnemonic
      return $ emitError $ CustomError msg (unpackSpan mnemonicN)
    else case filter isGoodOpndDesc mnemonicMatches of
           [] -> do
             let msg = printf "invalid operands for '%s' instruction (%s)" mnemonic (show opndDesc)
             return $ emitError (CustomError msg ss)
           [instrDesc] -> do
             er <- emitOpcode pass instrDesc
             er' <- emitOpnds pass (TD.operandDesc instrDesc) opnds
             return (er <> er')
           more -> do
             let msg =
                   printf
                     "instruction usage is ambiguous; target description is incorrect!\n%s"
                     (show more)
             return $ emitError (InternalCompilerError msg ss)

emitDataGroup ::
     Pass -> WithPos DataSize -> [WithPos (Expr WithPos)] -> State EmitState (EmitResult [Word8])
emitDataGroup RegisterLabelsPass dataSizeN dataNs =
  emitBinary $ replicate (byteSize * length dataNs) 0
  where
    byteSize = dataSizeBytes (unpackNode dataSizeN)
emitDataGroup EmitCodePass dataSizeN dataNs = checkedEmitDataGroup (unpackNode dataSizeN) dataNs

checkedEmitDataGroup :: DataSize -> [WithPos (Expr WithPos)] -> State EmitState (EmitResult [Word8])
checkedEmitDataGroup _ [] = return $ EmitOk []
checkedEmitDataGroup dataSize (dataN:rest) = do
  es <- get
  result <-
    case evalExpr es (unpackNode dataN) of
      EmitErr errs -> return $ EmitErr errs
      EmitOk val
        | val >= 2 ^ dataSizeBits dataSize -> do
          let msg = printf "data too large for %s (%d)" (show dataSize) val
          return $ emitError $ CustomError msg (unpackSpan dataN)
        | val < -(2 ^ dataSizeBits dataSize) -> do
          let msg = printf "data underflows %s (%d)" (show dataSize) val
          return $ emitError $ CustomError msg (unpackSpan dataN)
        | otherwise -> emitBinary $ dataLimbs (dataSizeBytes dataSize) val
  otherResults <- checkedEmitDataGroup dataSize rest
  return (result <> otherResults)

dataLimbs :: Int -> Int64 -> [Word8]
dataLimbs 0 _ = []
dataLimbs limbs val = fromIntegral (val .&. 0xFF) : dataLimbs (limbs - 1) (val `shiftR` 8)

getOpndDesc :: [WithPos (Operand WithPos)] -> Maybe TD.OperandDesc
getOpndDesc [] = Just TD.NoOpnd
getOpndDesc [opndN] =
  let opnd = unpackNode opndN
   in case opnd of
        Imm _ -> Just TD.ImmOpnd
        Addr _ -> Just TD.AddrOpnd
getOpndDesc _ = Nothing

emitOpcode :: Pass -> TD.Instruction -> State EmitState (EmitResult [Word8])
emitOpcode _ TD.Instr {TD.opcode = opcode} = emitBinary [opcode]

emitOpnds ::
     Pass -> TD.OperandDesc -> [WithPos (Operand WithPos)] -> State EmitState (EmitResult [Word8])
emitOpnds _ TD.NoOpnd [] = emitBinary []
emitOpnds RegisterLabelsPass TD.ImmOpnd [_] = emitBinary [0]
emitOpnds RegisterLabelsPass TD.AddrOpnd [_] = emitBinary [0, 0]
emitOpnds EmitCodePass TD.ImmOpnd [opndN]
  -- TODO(dsprenkels) Error on {over,under}flow
 = do
  es <- get
  let expr = unpackOpnd $ unpackNode opndN
  case evalExpr es expr of
    EmitOk val -> emitBinary [fromIntegral val]
    EmitErr errs -> return $ EmitErr errs
emitOpnds EmitCodePass TD.AddrOpnd [opndN]
  -- TODO(dsprenkels) Error on {over,under}flow
 = do
  es <- get
  let expr = unpackOpnd $ unpackNode opndN
  case evalExpr es expr of
    EmitOk val -> do
      let loByte = fromIntegral val
      let hiByte = fromIntegral (val `shiftR` 8)
      emitBinary [loByte, hiByte]
    EmitErr errs -> return $ EmitErr errs
emitOpnds _ _ _ = error "unreachable"

emitBinary :: [Word8] -> State EmitState (EmitResult [Word8])
emitBinary bin = do
  modify (<> mempty {offset = length bin})
  return $ emitSuccess bin

emitSuccess :: a -> EmitResult a
emitSuccess = EmitOk

emitError :: Error -> EmitResult a
emitError err = EmitErr (return err)

evalExpr :: EmitState -> Expr WithPos -> EmitResult Int64
evalExpr EmitState {maybeGlobalScope, cp} (Ident identN) = do
  let ident = inScope maybeGlobalScope (unpackNode identN)
  let ss = unpackSpan identN
  case cp Map.!? unpackNode identN of
    Just Value {value} -> emitSuccess value
    Nothing -> do
      let msg = printf "`%s` is not defined" ident
      emitError (CustomError msg ss)
evalExpr _ (Lit litN) = emitSuccess (unpackNode litN)
evalExpr es (Binary op leftN rightN) = do
  left <- evalExpr es (unpackNode leftN)
  right <- evalExpr es (unpackNode rightN)
  return $ getBinOp op left right
evalExpr es (Unary op opndN) = getUnOp op <$> evalExpr es (unpackNode opndN)

inScope :: Maybe Text -> Text -> Text
inScope Nothing name = name
inScope (Just scope) name
  | (not . isGlobal) name = scope <> name
  | otherwise = name

getBinOp :: BinOp -> (Int64 -> Int64 -> Int64)
getBinOp Add = (+)
getBinOp Sub = (-)
getBinOp Mul = (*)
getBinOp Div = quot
getBinOp Mod = rem
getBinOp And = (.&.)
getBinOp Or = (.|.)
getBinOp Xor = xor

getUnOp :: UnOp -> (Int64 -> Int64)
getUnOp Pos = id
getUnOp Neg = negate
getUnOp Not = complement
