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
    , bin :: [Word8]
    , cp :: ConstPool
    , errs :: [Error]
    }
  deriving (Show)

instance Semigroup EmitState where
  es1 <> es2 =
    EmitState
      { offset = offset es1 + offset es2
      , bin = bin es1 <> bin es2
      , cp = cp es1 <> cp es2
      , errs = errs es1 <> errs es2
      }

instance Monoid EmitState where
  mempty = EmitState {offset = 0, bin = mempty, cp = mempty, errs = mempty}

registerLabelsAST :: AST WithPos -> State EmitState ()
registerLabelsAST ast = modify (\es -> snd $ execState (registerLabelsAST' ast) (Nothing, es))

registerLabelsAST' :: AST WithPos -> State (Maybe Text, EmitState) ()
registerLabelsAST' [] = return mempty
registerLabelsAST' (decl:rest) = do
  (maybeGlobal, EmitState {cp, offset}) <- get
  case decl of
    LblDecl textN -> do
      let text = unpackNode textN
      let label = fromMaybe "" maybeGlobal <> text
      case Map.lookup text cp of
        Just _ -> do
          let msg = printf "symbol '%s' is already defined" label :: String
          let err = CustomError msg (unpackSpan textN)
          modify $ mapSnd (<> mempty {errs = [err]})
        Nothing -> do
          let val = Value {value = offset, ref = LabelRef textN}
          modify $ mapSnd (<> mempty {cp = Map.singleton text val})
          modify $
            mapFst
              (if isGlobal text
                 then const $ Just text
                 else id)
    InstrDecl instrN -> moveOffset (emitInstr instrN)
    DataDecl dataN -> moveOffset (emitData dataN)
  registerLabelsAST' rest
  where
    moveOffset :: State EmitState () -> State (Maybe Text, EmitState) ()
    moveOffset emitFn = do
      (maybeGlobal, es) <- get
      -- Try to build the code, but only register the change in offset
      let EmitState {offset} = execState emitFn es
      put (maybeGlobal, es {offset})

emit :: AST WithPos -> Either ErrorBundle ByteString
emit ast
  | not $ null $ errs esWithLabels = Left $ NE'.fromList $ errs esWithLabels
  | not $ null $ errs esComplete = Left $ NE'.fromList $ errs esComplete
  | otherwise = Right $ BS.pack $ bin esComplete
  where
    esWithLabels = execState (registerLabelsAST ast) mempty
    esComplete = execState (emitASTCode ast) (reset esWithLabels)
    -- | Throw away bad code information
    reset es = es {offset = 0, bin = mempty}

emitASTCode :: AST WithPos -> State EmitState ()
emitASTCode [] = return mempty
emitASTCode (decl:rest) = do
  es@EmitState {offset, cp} <- get
  case decl of
    LblDecl textN -> do
      let text = unpackNode textN
      -- Fill in the location of this label
      put es {cp = Map.adjust (\val -> val {value = offset}) text cp}
    InstrDecl instrN -> emitInstr instrN
    DataDecl dataN -> emitData dataN
  emitASTCode rest

emitInstr :: WithPos (Instruction WithPos) -> State EmitState ()
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
    else case filter isGoodOpndDesc mnemonicMatches of
           [] -> do
             let msg = printf "invalid operands for '%s' instruction (%s)" mnemonic (show opndDesc)
             emitError $ CustomError msg ss
           [instrDesc] -> do
             emitOpcode instrDesc
             emitOpnds (TD.operandDesc instrDesc) opnds
           more -> do
             let msg =
                   printf
                     "instruction usage is ambiguous; target description is incorrect!\n%s"
                     (show more)
             emitError $ InternalCompilerError msg ss

emitData :: WithPos (Expr WithPos) -> State EmitState ()
emitData dataN = do
  data_ <- evalExpr (unpackNode dataN)
  if not (0 <= data_ && data_ < 256)
    then do
      let msg = printf "data should be a valid byte value (%d)" data_
      emitError $ CustomError msg (unpackSpan dataN)
    else emitBinary [fromIntegral data_]

getOpndDesc :: [WithPos (Operand WithPos)] -> Maybe TD.OperandDesc
getOpndDesc [] = Just TD.NoOpnd
getOpndDesc [opndN] =
  let opnd = unpackNode opndN
   in case opnd of
        Imm _ -> Just TD.ImmOpnd
        Addr _ -> Just TD.AddrOpnd
getOpndDesc _ = Nothing

emitOpcode :: TD.Instruction -> State EmitState ()
emitOpcode TD.Instr {TD.opcode = opcode} = emitBinary [opcode]

emitOpnds :: TD.OperandDesc -> [WithPos (Operand WithPos)] -> State EmitState ()
emitOpnds TD.NoOpnd [] = return ()
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

emitBinary :: [Word8] -> State EmitState ()
emitBinary bin = modify (<> mempty {offset = length bin, bin})

emitError :: Error -> State EmitState ()
emitError err = modify (<> mempty {errs = [err]})

evalExpr :: Expr WithPos -> State EmitState Int
evalExpr (Ident identN) = do
  let ident = unpackNode identN
  let ss = unpackSpan identN
  EmitState {cp} <- get
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
