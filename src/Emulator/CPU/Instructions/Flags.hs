module Emulator.CPU.Instructions.Flags where

import Emulator.CPU
import Emulator.Interpreter.Monad
import Emulator.Types

import Control.Lens hiding (op)
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

arithmeticAddOverflow :: MWord -> MWord -> MWord -> Bool
arithmeticAddOverflow a b y = (not (a `testBit` 31) || (b `testBit` 31)) && y `testBit` 31

arithmeticNegative :: MWord -> Bool
arithmeticNegative y = y `testBit` 31

arithmeticCarry :: (Word64 -> Word64 -> Word64) -> Word32 -> Word32 -> Bool
arithmeticCarry op a b = (op (fromIntegral a) (fromIntegral b)) `testBit` 32

arithmeticZero :: MWord -> Bool
arithmeticZero 0 = True
arithmeticZero _ = False
