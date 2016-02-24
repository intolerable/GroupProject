module Emulator.CPU.Instructions.Parser where

data CPUMode = ARM | THUMB
  deriving (Show, Read, Eq)

type ARM = 'ARM
type THUMB = 'THUMB

type Accumulate = ()
type Link = ()
type Offset = ()
type RegisterName = ()
type SetCondition = ()

data Instruction a where
  Multiply :: Accumulate -> SetCondition -> RegisterName -> RegisterName -> RegisterName -> RegisterName -> Instruction ARM
    -- MUL A S Rd Rn Rs Rm, Rd <- Rm * Rs (+ Rn if A)
  Branch :: Link -> Offset -> Instruction ARM
  BranchExchange :: RegisterName -> Instruction ARM
