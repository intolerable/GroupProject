module Emulator.CPU.Instructions.THUMB.Opcodes where

import Emulator.CPU hiding (CPUMode(..), Interrupt(..))
import Emulator.CPU.Instructions.THUMB
import Emulator.CPU.Instructions.Types
import Emulator.CPU.Instructions.Flags
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
  T_ROR -> tRor
  T_TST -> tTst
  T_NEG -> tNeg
  T_CMP -> tCmp
  T_CMN -> tCmn
  T_ORR -> tOrr
  T_MUL -> tMul
  T_BIC -> tBic
  T_MVN -> tMvn
  T_MOV -> error "Mov passed to THUMB ALU operation"
  T_ADD -> error "Add passed to THUMB ALU operation"

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
  flags.negative .= arithmeticNegative val
  flags.zero .= arithmeticZero val
  flags.carry .= arithmeticCarry (+) (fromIntegral v) (fromIntegral (v'+cy))
  flags.overflow .= arithmeticAddOverflow v (v' + cy) val

tSbc :: IsSystem s m => RegisterName -> RegisterName -> m ()
tSbc src dest = do
  v <- use (registers.rn src)
  v' <- use (registers.rn dest)
  fcy <- use $ flags.carry
  let cy = if fcy then 0 else 1
  let val = v' - v - cy
  registers.rn dest .= val
  flags.negative .= arithmeticNegative val
  flags.zero .= arithmeticZero val
  flags.carry .= arithmeticCarry (-) (fromIntegral v') (fromIntegral (v-cy))
  flags.overflow .= False

tTst :: IsSystem s m => RegisterName -> RegisterName -> m ()
tTst src src' = do
  v1 <- use $ registers.rn src
  v2 <- use $ registers.rn src'
  let val = v1 .&. v2
  setFlagsLogic val

tNeg :: IsSystem s m => RegisterName -> RegisterName -> m ()
tNeg src dest = do
  val <- use $ registers.rn src
  let newVal = complement val
  registers.rn dest .= newVal
  setFlagsLogic newVal

tCmp :: IsSystem s m => RegisterName -> RegisterName -> m ()
tCmp src dest = do
  v <- use $ registers.rn dest
  v' <- use $ registers.rn src
  let val = v - v'
  setFlagsLogic val
  flags.carry .= arithmeticCarry (-) v v'
  flags.overflow .= False

tCmn :: IsSystem s m => RegisterName -> RegisterName -> m ()
tCmn src dest = do
  v <- use $ registers.rn dest
  v' <- use $ registers.rn src
  let val = v - v'
  setFlagsLogic val
  flags.carry .= arithmeticCarry (+) v v'
  flags.overflow .= arithmeticAddOverflow v v' val

tOrr :: IsSystem s m => RegisterName -> RegisterName -> m ()
tOrr src dest = do
  v <- use $ registers.rn src
  v' <- use $ registers.rn dest
  let val = v .|. v'
  registers.rn dest .= val
  setFlagsLogic val

tMul :: IsSystem s m => RegisterName -> RegisterName -> m ()
tMul src dest = do
  v <- use $ registers.rn src
  v' <- use $ registers.rn dest
  let val = v * v'
  registers.rn dest .= val
  setFlagsLogic val
  flags.carry .= arithmeticCarry (*) (fromIntegral v) (fromIntegral v')
  flags.overflow .= arithmeticAddOverflow v v' val -- THIS ISNT RIGHT!!!! ---

tBic :: IsSystem s m => RegisterName -> RegisterName -> m ()
tBic src dest = do
  v <- use $ registers.rn src
  v' <- use $ registers.rn dest
  let val = v' .&. (complement v)
  registers.rn dest .= val
  setFlagsLogic val

tMvn :: IsSystem s m => RegisterName -> RegisterName -> m ()
tMvn src dest = do
  v <- use $ registers.rn src
  registers.rn dest .= (complement v)
  setFlagsLogic v

tRor :: IsSystem s m => RegisterName -> RegisterName -> m ()
tRor src dest = do
  v <- use $ registers.rn src
  v' <- use $ registers.rn dest
  cy <- use $ flags.carry
  let (newCy, val) = applyShiftTypeWithCarry RotateRight v' (fromIntegral v) cy
  registers.rn dest .= val
  setFlagsLogic val
  flags.carry .= newCy
  flags.overflow .= undefined
