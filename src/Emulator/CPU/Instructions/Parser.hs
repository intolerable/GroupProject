module Emulator.CPU.Instructions.Parser where

import Emulator.CPU
import Emulator.Types

data CPUMode = ARM | THUMB
  deriving (Show, Read, Eq)

type ARM = 'ARM
type THUMB = 'THUMB

type Accumulate = ()
type Link = ()
type Offset = ()
type RegisterName = ()
type SetCondition = ()
type Rotated a = a

data Instruction a where
  Multiply :: Accumulate -> SetCondition -> RegisterName -> RegisterName -> RegisterName -> RegisterName -> Instruction ARM
    -- MUL A S Rd Rn Rs Rm, Rd <- Rm * Rs (+ Rn if A)
  Branch :: Link -> Offset -> Instruction ARM
  BranchExchange :: RegisterName -> Instruction ARM
  DataProcessing :: Opcode -> SetCondition -> RegisterName -> RegisterName -> Either (Shifted RegisterName) (Rotated Byte) -> Instruction ARM
