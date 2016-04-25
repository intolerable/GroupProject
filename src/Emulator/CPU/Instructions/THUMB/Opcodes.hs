module Emulator.CPU.Instructions.THUMB.Opcodes where

import Emulator.CPU hiding (CPUMode(..), Interrupt(..))
import Emulator.CPU.Instructions.THUMB
import Emulator.CPU.Instructions.Types
import Emulator.Interpreter.Monad
import Emulator.Types

import Control.Lens hiding (op)
import Data.Bits

functionFromOpcode :: IsSystem s m => ThumbOpcode -> (RegisterName -> RegisterName -> m ())
functionFromOpcode op = case op of
  T_AND -> tAnd
  T_EOR -> tEor
  T_LSL -> tLsl
  T_LSR -> tLsr
  T_ASR -> undefined
  T_ADC -> undefined
  T_SBC -> undefined
  T_ROR -> undefined
  T_TST -> undefined
  T_NEG -> undefined
  T_CMP -> undefined
  T_CMN -> undefined
  T_ORR -> undefined
  T_MUL -> undefined
  T_BIC -> undefined
  T_MVN -> undefined
  T_ADD -> undefined
  T_MOV -> error "Mov passed to ALU operation"

setFlagsLogic :: IsSystem s m => MWord -> m ()
setFlagsLogic v = do
  flags.zero .= (v == 0)
  flags.negative .= testBit v 15

setShiftFlags :: IsSystem s m => ShiftType -> MWord -> MWord -> Int -> m ()
setShiftFlags t v v' n = do
  setFlagsLogic v'
  case t of
    LogicalLeft -> flags.carry .= testBit v (15-(n-1))
    LogicalRight -> flags.carry .= testBit v (n-1)
    _ -> error "Uninimplemented shift type for setShiftFlags:Thumb.Opcodes"


tAnd :: IsSystem s m => RegisterName -> RegisterName -> m ()
tAnd src dest = do
  srcV <- use (registers.rn src)
  srcV' <- use (registers.rn dest)
  let val = srcV .&. srcV'
  registers.rn dest .= val
  setFlagsLogic val

tEor :: IsSystem s m => RegisterName -> RegisterName -> m ()
tEor src dest = do
  srcV <- use (registers.rn src)
  srcV' <- use (registers.rn dest)
  let val = srcV `xor` srcV'
  registers.rn dest .= val
  setFlagsLogic val

tLsl :: IsSystem s m => RegisterName -> RegisterName -> m ()
tLsl src dest = do
  srcV <- use (registers.rn src)
  srcV' <- use (registers.rn dest)
  let val = srcV' `shiftL` (fromIntegral srcV)
  registers.rn dest .= val
  setShiftFlags LogicalLeft srcV' val $ fromIntegral srcV

tLsr :: IsSystem s m => RegisterName -> RegisterName -> m ()
tLsr src dest = do
  v <- use (registers.rn src)
  v' <- use (registers.rn dest)
  let val = v' `shiftR` fromIntegral v
  registers.rn dest .= val
  setShiftFlags LogicalRight v' val $ fromIntegral v
