module Emulator.CPU.Instructions.Flags where

import Emulator.CPU
import Emulator.Interpreter.Monad
import Emulator.Types

import Control.Lens
import Data.Bits
import Data.Word

setFlagsLogic :: IsSystem s m => MWord -> m ()
setFlagsLogic v = do
  flags.zero .= (v == 0)
  flags.negative .= testBit v 31

setShiftFlags :: IsSystem s m => ShiftType -> MWord -> MWord -> Int -> m ()
setShiftFlags t v v' n = do
  setFlagsLogic v'
  case t of
    LogicalLeft -> flags.carry .= testBit v (15-(n-1))
    LogicalRight -> flags.carry .= testBit v (n-1)
    _ -> error "Uninimplemented shift type for setShiftFlags:Thumb.Opcodes"

wouldCarry :: (Word64 -> Word64 -> Word64) -> Word64 -> Word64 -> Bool
wouldCarry op a b = ((op a b) .&. 0xFFFFFFFF00000000) > 0

isOverflow :: MWord -> Bool
isOverflow v = v > 0x7FFFFFFF
