{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TargetDesc where

import qualified Asm
import qualified AST
import           Data.Bits
import           RIO
import           RIO.List         (uncons)
import           RIO.List.Partial (head)
import           Text.Printf      (printf)

type FmtInstr = Instruction -> AST.Instruction Asm.WithPos -> [Word8]

type MatchInstr = AST.Instruction Asm.WithPos -> Bool

data Instruction = Instr
    { opcode   :: !Word8
    , mnemonic :: !Text
    , match    :: AST.Instruction Asm.WithPos -> Bool
    , format   :: FmtInstr
    }

fAnd :: Applicative f => f Bool -> f Bool -> f Bool
fAnd = liftA2 (&&)

fOr :: Applicative f => f Bool -> f Bool -> f Bool
fOr = liftA2 (||)

fromRegName :: Text -> Word8
fromRegName "r0"    = 0
fromRegName "r1"    = 1
fromRegName "r2"    = 2
fromRegName "r3"    = 3
fromRegName "r4"    = 4
fromRegName "r5"    = 5
fromRegName "sp"    = 6
fromRegName "pc"    = 7
fromRegName regName = error $ printf "invalid register name: %s" regName

checkOpcode :: Word8 -> Word8
checkOpcode opcode
  | (opcode .&. complement 0x1F) /= 0 =
    error "opcode is more than 5 bits wide (0x%02x)" opcode
  | otherwise = opcode

checkOpndsCount ::
     Int -> AST.Instruction Asm.WithPos -> AST.Instruction Asm.WithPos
checkOpndsCount expected AST.Instr {AST.mnemonic, AST.opnds}
  | length opnds /= expected =
    error $
    printf
      "mnemonic '%s' expects exactly %d operand (%s)"
      expected
      (show mnemonic)
      (show opnds)
  | otherwise = AST.Instr {AST.mnemonic, AST.opnds}

fmtEmpty :: FmtInstr
fmtEmpty Instr {opcode} AST.Instr {AST.mnemonic, AST.opnds}
  | not $ null opnds =
    error $
    printf "mnemonic '%s' expects no operands (%s)" (show mnemonic) (show opnds)
  | otherwise = [checkOpcode opcode]

fmtRegister :: FmtInstr
fmtRegister Instr {opcode} instruction =
  let AST.Instr {AST.opnds} = checkOpndsCount 1 instruction
      regNode =
        case head (Asm.unpackNode <$> opnds) of
          AST.OpR inner -> inner
          _             -> error "unreachable"
      AST.Reg reg = Asm.unpackNode regNode
   in [checkOpcode opcode .|. (fromRegName reg `shiftL` 5)]

fmtImmediate :: FmtInstr
fmtImmediate Instr {opcode} instruction =
  let AST.Instr {AST.opnds} = checkOpndsCount 1 instruction
      immNode =
        case head (Asm.unpackNode <$> opnds) of
          AST.OpI inner -> inner
          _             -> error "unreachable"
      AST.Imm imm = Asm.unpackNode immNode
   in [checkOpcode opcode, imm]

fmtAddress :: FmtInstr
fmtAddress Instr {opcode} instruction =
  let AST.Instr {AST.opnds} = checkOpndsCount 1 instruction
      addrNode =
        case head (Asm.unpackNode <$> opnds) of
          AST.OpA inner -> inner
          _             -> error "unreachable"
      AST.Addr addr = Asm.unpackNode addrNode
   in checkOpcode opcode : fmtWord16 addr
  where
    fmtWord16 w = fromIntegral <$> [w .&. 0xFF, (w `shiftR` 8) .&. 0xFF]

isMnemonic :: Text -> MatchInstr
isMnemonic tdMnemonic instrNode =
  let AST.Instr {AST.mnemonic} = instrNode
   in Asm.unpackNode mnemonic == tdMnemonic

hasImmediateOpnd :: MatchInstr
hasImmediateOpnd instrNode =
  let AST.Instr {AST.opnds} = instrNode
   in case uncons $ Asm.unpackNode <$> opnds of
        Just (AST.OpI _, _) -> True
        _                   -> False

hasRegisterOpnd :: MatchInstr
hasRegisterOpnd instrNode =
  let AST.Instr {AST.opnds} = instrNode
   in case uncons $ Asm.unpackNode <$> opnds of
        Just (AST.OpR _, _) -> True
        _                   -> False

hasAddressOpnd :: MatchInstr
hasAddressOpnd instrNode =
  let AST.Instr {AST.opnds} = instrNode
   in case uncons $ Asm.unpackNode <$> opnds of
        Just (AST.OpL _, _) -> True
        Just (AST.OpA _, _) -> True
        _                   -> False

bebbInstrs :: [Instruction]
bebbInstrs =
  [ Instr
      { opcode = 0x00
      , mnemonic = "brk"
      , match = isMnemonic "brk"
      , format = fmtEmpty
      }
  , Instr
      { opcode = 0x01
      , mnemonic = "nop"
      , match = isMnemonic "nop"
      , format = fmtEmpty
      }
  , Instr
      { opcode = 0x02
      , mnemonic = "lda"
      , match = isMnemonic "lda"
      , format = fmtAddress
      }
  , Instr
      { opcode = 0x03
      , mnemonic = "sta"
      , match = isMnemonic "sta"
      , format = fmtAddress
      }
  , Instr
      { opcode = 0x04
      , mnemonic = "jmp"
      , match = isMnemonic "jmp"
      , format = fmtAddress
      }
  , Instr
      { opcode = 0x05
      , mnemonic = "jz"
      , match = isMnemonic "jz"
      , format = fmtAddress
      }
  , Instr
      { opcode = 0x06
      , mnemonic = "jc"
      , match = isMnemonic "jc"
      , format = fmtAddress
      }
  , Instr
      { opcode = 0x07
      , mnemonic = "out"
      , match = isMnemonic "out"
      , format = fmtEmpty
      }
  , Instr -- Addition with address value and add carry bit
      { opcode = 0x08
      , mnemonic = "adc"
      , match = isMnemonic "adc" `fAnd` hasAddressOpnd
      , format = fmtAddress
      }
  , Instr -- Subtraction with address value and subtract borrow bit (= !carrybit)
      { opcode = 0x09
      , mnemonic = "subc"
      , match = isMnemonic "subc" `fAnd` hasAddressOpnd
      , format = fmtAddress
      }
  , Instr -- Bitwise OR with address value
      { opcode = 0x0A
      , mnemonic = "or"
      , match = isMnemonic "or" `fAnd` hasAddressOpnd
      , format = fmtAddress
      }
  , Instr -- Bitwise AND with address value
      { opcode = 0x0B
      , mnemonic = "and"
      , match = isMnemonic "and" `fAnd` hasAddressOpnd
      , format = fmtAddress
      }
  , Instr -- Bitwise XOR with address value
      { opcode = 0x0C
      , mnemonic = "xor"
      , match = isMnemonic "xor" `fAnd` hasAddressOpnd
      , format = fmtAddress
      }
  , Instr -- Bitwise XOR with address value
      { opcode = 0x0D
      , mnemonic = "not"
      , match = isMnemonic "not" `fAnd` hasAddressOpnd
      , format = fmtEmpty
      }
  , Instr
      { opcode = 0x0E
      , mnemonic = "clc"
      , match = isMnemonic "clc"
      , format = fmtEmpty
      }
  , Instr
      { opcode = 0x0F
      , mnemonic = "clz"
      , match = isMnemonic "clz"
      , format = fmtEmpty
      }
  , Instr
      { opcode = 0x10
      , mnemonic = "push"
      , match = isMnemonic "push"
      , format = fmtEmpty
      }
  , Instr
      { opcode = 0x11
      , mnemonic = "pop"
      , match = isMnemonic "pop"
      , format = fmtEmpty
      }
  , Instr
      { opcode = 0x12
      , mnemonic = "call"
      , match = isMnemonic "call"
      , format = fmtAddress
      }
  , Instr
      { opcode = 0x13
      , mnemonic = "ret"
      , match = isMnemonic "ret"
      , format = fmtEmpty
      }
  ]
-- push
-- pop
-- jmp
-- brk
-- adc [addr]
-- sbc [addr]
-- or [addr]
-- and [addr]
-- xor [addr]
-- lda [addr]
-- sta [addr]
-- jz [addr]
-- jc [addr]
-- call [addr]
-- ret
-- out
-- clc
-- clz
-- lda [reg]
-- lda [imm]
-- sta [reg]
-- lda [addr]
-- sta [addr]
-- xor [reg]
-- xor [imm]
-- or [reg]
-- or [imm]
-- and [reg]
-- and [imm]
-- jmp
-- jz (je)
-- jnz (jne)
-- jc (jnb)
-- jb (jnc)
