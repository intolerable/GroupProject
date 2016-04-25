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
  T_EOR -> undefined
  T_LSL -> undefined
  T_LSR -> undefined
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
  flags.carry .= False
  flags.overflow .= False

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
