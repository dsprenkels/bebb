{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TargetDesc where

import           Data.Bits
import           RIO
import           Text.Printf                    ( printf )

type FmtInstr = Instruction -> Instruction -> [Word8]

data Instruction = Instr
    { opcode   :: !Word8
    , mnemonic :: !Text
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
checkOpcode instrOpcode
  | (instrOpcode .&. complement 0x1F) /= 0 = error
    "instrOpcode is more than 5 bits wide (0x%02x)"
    instrOpcode
  | otherwise = instrOpcode

check :: [Instruction] -> [Instruction]
check instrs = checkOpcode . opcode <$> instrs *> instrs

bebbInstrs :: [Instruction]
bebbInstrs = check
  [ Instr { opcode = 0x00, mnemonic = "brk" }
  , Instr { opcode = 0x01, mnemonic = "nop" }
  , Instr { opcode = 0x02, mnemonic = "lda" }
  , Instr { opcode = 0x03, mnemonic = "sta" }
  , Instr { opcode = 0x04, mnemonic = "jmp" }
  , Instr { opcode = 0x05, mnemonic = "jz" }
  , Instr { opcode = 0x06, mnemonic = "jc" }
  , Instr { opcode = 0x07, mnemonic = "out" }
  -- Addition with address value and add carry bit
  , Instr { opcode = 0x08, mnemonic = "adc" }
  -- Subtraction with address value and subtract borrow bit (= !carrybit)
  , Instr { opcode = 0x09, mnemonic = "subc" }
  -- Bitwise OR with address value
  , Instr { opcode = 0x0A, mnemonic = "or" }
  -- Bitwise AND with address value
  , Instr { opcode = 0x0B, mnemonic = "and" }
  -- Bitwise XOR with address value
  , Instr { opcode = 0x0C, mnemonic = "xor" }
  -- Bitwise XOR with address value
  , Instr { opcode = 0x0D, mnemonic = "not" }
  , Instr { opcode = 0x0E, mnemonic = "clc" }
  , Instr { opcode = 0x0F, mnemonic = "clz" }
  , Instr { opcode = 0x10, mnemonic = "push" }
  , Instr { opcode = 0x11, mnemonic = "pop" }
  , Instr { opcode = 0x12, mnemonic = "call" }
  , Instr { opcode = 0x13, mnemonic = "ret" }
  ]
