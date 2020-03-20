{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TargetDesc where

import           Data.Bits
import           RIO
import           RIO.Map


data OperandDesc = NoOpnd | ImmOpnd | AddrOpnd deriving (Show, Eq, Enum)

data Instruction = Instr
    { opcode   :: !Word8
    , mnemonic :: !Text
    , operandDesc :: !OperandDesc
    }

fAnd :: Applicative f => f Bool -> f Bool -> f Bool
fAnd = liftA2 (&&)

fOr :: Applicative f => f Bool -> f Bool -> f Bool
fOr = liftA2 (||)

checkOpcode :: Word8 -> Word8
checkOpcode instrOpcode
  | (instrOpcode .&. complement 0x1F) /= 0 = error
    "instrOpcode is more than 5 bits wide (0x%02x)"
    instrOpcode
  | otherwise = instrOpcode

check :: [Instruction] -> [Instruction]
check instrs = checkOpcode . opcode <$> instrs *> instrs

instrSize :: Instruction -> Int
instrSize Instr { operandDesc = NoOpnd }   = 1
instrSize Instr { operandDesc = ImmOpnd }  = 2
instrSize Instr { operandDesc = AddrOpnd } = 3

bebbInstrs :: [Instruction]
bebbInstrs = check
  [ Instr { opcode = 0x00, mnemonic = "nop", operandDesc = NoOpnd }
  , Instr { opcode = 0x01, mnemonic = "brk", operandDesc = NoOpnd }
  , Instr { opcode = 0x02, mnemonic = "lda", operandDesc = AddrOpnd }
  , Instr { opcode = 0x03, mnemonic = "lda", operandDesc = ImmOpnd }
  , Instr { opcode = 0x04, mnemonic = "adc", operandDesc = AddrOpnd }
  , Instr { opcode = 0x05, mnemonic = "adc", operandDesc = ImmOpnd }
  -- Subtraction with address value and subtract borrow bit (= !carrybit)
  , Instr { opcode = 0x06, mnemonic = "subc", operandDesc = AddrOpnd }
  , Instr { opcode = 0x07, mnemonic = "subc", operandDesc = ImmOpnd }
  -- Bitwise OR with address value
  , Instr { opcode = 0x08, mnemonic = "or", operandDesc = AddrOpnd }
  , Instr { opcode = 0x09, mnemonic = "or", operandDesc = ImmOpnd }
  -- Bitwise AND with address value
  , Instr { opcode = 0x0A, mnemonic = "and", operandDesc = AddrOpnd }
  , Instr { opcode = 0x0B, mnemonic = "and", operandDesc = ImmOpnd }
  -- Bitwise XOR with address value
  , Instr { opcode = 0x0C, mnemonic = "xor", operandDesc = AddrOpnd }
  , Instr { opcode = 0x0D, mnemonic = "xor", operandDesc = ImmOpnd }
  -- Bitwise NOT the value in A
  , Instr { opcode = 0x0E, mnemonic = "not", operandDesc = NoOpnd }
  , Instr { opcode = 0x0F, mnemonic = "sta", operandDesc = AddrOpnd }
  , Instr { opcode = 0x10, mnemonic = "jmp", operandDesc = AddrOpnd }
  , Instr { opcode = 0x11, mnemonic = "jz", operandDesc = AddrOpnd }
  , Instr { opcode = 0x12, mnemonic = "jc", operandDesc = AddrOpnd }
  , Instr { opcode = 0x13, mnemonic = "clc", operandDesc = NoOpnd }
  , Instr { opcode = 0x14, mnemonic = "clz", operandDesc = NoOpnd }
  , Instr { opcode = 0x15, mnemonic = "push", operandDesc = NoOpnd }
  , Instr { opcode = 0x16, mnemonic = "pop", operandDesc = NoOpnd }
  , Instr { opcode = 0x17, mnemonic = "call", operandDesc = AddrOpnd }
  , Instr { opcode = 0x18, mnemonic = "ret", operandDesc = NoOpnd }
  , Instr { opcode = 0x19, mnemonic = "out", operandDesc = NoOpnd }
  ]
