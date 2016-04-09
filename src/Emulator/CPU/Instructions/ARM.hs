module Emulator.CPU.Instructions.ARM where

import Emulator.CPU
import Emulator.CPU.Instructions.Types
import Emulator.Types

data ARMInstruction
  = DataProcessing Opcode SetCondition RegisterName RegisterName (Either (Shifted RegisterName) (Rotated Byte))
  | Multiply Accumulate SetCondition RegisterName RegisterName RegisterName RegisterName
  | MultiplyLong Signed Accumulate SetCondition RegisterName RegisterName RegisterName RegisterName
  | SingleDataSwap (Granularity 'Full) RegisterName RegisterName RegisterName
  | BranchExchange RegisterName
  | HalfwordDataTransferRegister PrePost OffsetDirection WriteBack LoadStore Signed (Granularity 'Lower) RegisterName RegisterName RegisterName
  | HalfwordDataTransferImmediate PrePost OffsetDirection WriteBack LoadStore Signed (Granularity 'Lower) RegisterName RegisterName Offset
  | SingleDataTransfer PrePost OffsetDirection (Granularity 'Full) WriteBack LoadStore RegisterName RegisterName (Either (Shifted RegisterName) Offset)
  | Undefined
  | BlockDataTransfer PrePost OffsetDirection ForceUserMode WriteBack LoadStore RegisterName RegisterList
  | Branch Link BranchOffset
  | CoprocessorDataTransfer
  | CoprocessorDataOperation
  | CoprocessorRegisterTransfer
  | SoftwareInterrupt
  deriving (Show, Read, Eq)
