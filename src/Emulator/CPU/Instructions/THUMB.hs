module Emulator.CPU.Instructions.THUMB where

import Emulator.CPU
import Emulator.CPU.Instructions.Types
import Emulator.Types

data THUMBInstruction
  = MoveShiftedRegister (Shifted RegisterName) RegisterName
  | AddSubtractImmediate AddSub Offset RegisterName RegisterName
  | AddSubtractRegister AddSub RegisterName RegisterName RegisterName
  | MovCmpAddSubImmediate Opcode RegisterName Offset
  | ALUOperation ThumbOpcode RegisterName RegisterName
  | HiRegOperation ThumbOpcode RegisterName RegisterName
  | ThumbBranchExchange RegisterName
  | PCRelativeLoad RegisterName Offset
  | ThumbLoadStoreRegisterOffset LoadStore (Granularity 'Full) RegisterName RegisterName RegisterName
  | ThumbLoadStoreSignExtHalfwordByte (Granularity 'Lower) LoadStore SignExtended RegisterName RegisterName RegisterName
  | ThumbLoadStoreImmediateOffset (Granularity 'Full) LoadStore Offset RegisterName RegisterName
  | ThumbLoadStoreHalfword LoadStore Offset RegisterName RegisterName
  | SPRelativeLoadStore LoadStore RegisterName Offset
  | LoadAddress BaseSource RegisterName Offset
  | SPAddOffset OffsetDirection Offset
  | PushPopRegs LoadStore StoreLR RegisterList
  | MultipleLoadStore LoadStore RegisterName RegisterList
  | ConditionalBranch Condition Offset
  | ThumbSoftwareInterrupt Value
  | ThumbBranch BranchOffset
  | LongBranchWLink LowHigh HalfWord
  deriving (Show, Eq)
