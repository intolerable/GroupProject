module Emulator.CPU.Instructions.THUMB.Opcodes where

import Emulator.CPU hiding (CPUMode(..), Interrupt(..))
import Emulator.CPU.Instructions.ARM.Opcodes (isSignedOverflow, isUnsignedOverflow)
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
  T_ASR -> tAsr
  T_ADC -> tAdc
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
  flags.negative .= testBit v 31

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

tAsr :: IsSystem s m => RegisterName -> RegisterName -> m ()
tAsr src dest = do
  v <- use (registers.rn src)
  v' <- use (registers.rn dest)
  let msb = testBit v' 31
  let val = v' `shiftR` fromIntegral v
  let x = if msb then val .|. 0x80000000
          else val .&. 0x7FFFFFFF
  registers.rn dest .= x
  setShiftFlags LogicalRight v' x $ fromIntegral v

tAdc :: IsSystem s m => RegisterName -> RegisterName -> m ()
tAdc src dest = do
  v <- use (registers.rn src)
  v' <- use (registers.rn dest)
  fcy <- use $ flags.carry
  let cy = if fcy then 1 else 0
  let val = v + v' + cy
  registers.rn dest .= val
  flags.negative .= testBit val 31
  flags.zero .= (val == 0)
  flags.carry .= isUnsignedOverflow (+) [v, v', cy] val
  flags.overflow .= isSignedOverflow (+) [v, v', cy] val

