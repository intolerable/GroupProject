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
type Signed = ()
type Granularity = ()

data Instruction a where
  DataProcessing :: Opcode -> SetCondition -> RegisterName -> RegisterName -> Either (Shifted RegisterName) (Rotated Byte) -> Instruction ARM
  Multiply :: Accumulate -> SetCondition -> RegisterName -> RegisterName -> RegisterName -> RegisterName -> Instruction ARM
    -- MUL A S Rd Rn Rs Rm, Rd <- Rm * Rs (+ Rn if A)
  MultiplyLong :: Signed -> Accumulate -> SetCondition -> RegisterName -> RegisterName -> RegisterName -> RegisterName -> Instruction ARM
  SingleDataSwap :: Granularity -> RegisterName -> RegisterName -> RegisterName -> Instruction ARM
  BranchExchange :: RegisterName -> Instruction ARM
  Branch :: Link -> Offset -> Instruction ARM
  Undefined :: Instruction ARM
