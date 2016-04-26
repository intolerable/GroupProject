module Emulator.CPU.Instructions.THUMB.Opcodes where

import Emulator.CPU hiding (CPUMode(..), Interrupt(..))
import Emulator.CPU.Instructions.ARM.Opcodes (isSignedOverflow, isUnsignedOverflow)
import Emulator.CPU.Instructions.THUMB
import Emulator.CPU.Instructions.Types
import Emulator.Interpreter.Monad
import Emulator.Types

import Control.Lens hiding (op)
import Data.Bits
import Data.Word

functionFromOpcode :: IsSystem s m => ThumbOpcode -> (RegisterName -> RegisterName -> m ())
functionFromOpcode op = case op of
  T_AND -> tAnd
  T_EOR -> tEor
  T_LSL -> tLsl
  T_LSR -> tLsr
  T_ASR -> tAsr
  T_ADC -> tAdc
  T_SBC -> tSbc
  T_ROR -> undefined
  T_TST -> tTst
  T_NEG -> tNeg
  T_CMP -> tCmp
  T_CMN -> tCmn
  T_ORR -> tOrr
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

wouldCarry :: (Word64 -> Word64 -> Word64) -> Word64 -> Word64 -> Bool
wouldCarry op a b = ((op a b) .&. 0xFFFFFFFF00000000) > 0

isOverflow :: MWord -> Bool
isOverflow v = v > 0x7FFFFFFF

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
  flags.carry .= wouldCarry (+) (fromIntegral v) (fromIntegral (v'+cy))
  flags.overflow .= isOverflow val

tSbc :: IsSystem s m => RegisterName -> RegisterName -> m ()
tSbc src dest = do
  v <- use (registers.rn src)
  v' <- use (registers.rn dest)
  fcy <- use $ flags.carry
  let cy = if fcy then 0 else 1
  let val = v' - v - cy
  registers.rn dest .= val
  flags.negative .= testBit val 31
  flags.zero .= (val == 0)
  flags.carry .= wouldCarry (-) (fromIntegral v') (fromIntegral (v-cy))
  flags.overflow .= isOverflow val

tTst :: IsSystem s m => RegisterName -> RegisterName -> m ()
tTst src src' = do
  v1 <- use $ registers.rn src
  v2 <- use $ registers.rn src'
  let val = v1 .&. v2
  setFlagsLogic val

tNeg :: IsSystem s m => RegisterName -> RegisterName -> m ()
tNeg dest src = do
  val <- use $ registers.rn src
  let newVal = negate val
  registers.rn dest .= newVal
  setFlagsLogic newVal


doCmp :: IsSystem s m => (MWord -> MWord -> MWord) -> (Word64 -> Word64 -> Word64) -> RegisterName -> RegisterName -> m ()
doCmp op bigOp dest src = do
  v <- use $ registers.rn dest
  v' <- use $ registers.rn src
  let val = op v v'
  setFlagsLogic val
  flags.carry .= wouldCarry bigOp (fromIntegral v) (fromIntegral v')
  flags.overflow .= isOverflow val

tCmp :: IsSystem s m => RegisterName -> RegisterName -> m ()
tCmp = doCmp (-) (-)

tCmn :: IsSystem s m => RegisterName -> RegisterName -> m ()
tCmn = doCmp (+) (+)

tOrr :: IsSystem s m => RegisterName -> RegisterName -> m ()
tOrr src dest = do
  v <- use $ registers.rn src
  v' <- use $ registers.rn dest
  let val = v .|. v'
  registers.rn dest .= val
  setFlagsLogic val
