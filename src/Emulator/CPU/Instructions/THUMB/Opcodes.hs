module Emulator.CPU.Instructions.THUMB.Opcodes where

import Emulator.CPU hiding (CPUMode(..), Interrupt(..))
import Emulator.CPU.Instructions.THUMB
import Emulator.CPU.Instructions.Types
import Emulator.Interpreter.Monad
import Emulator.Types

import Control.Lens hiding (op)
import Data.Bits

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

tAnd :: Monad m => RegisterName -> RegisterName -> SystemT m ()
tAnd src dest = do
  src <- use (registers.rn src)
  src' <- use (registers.rn dest)
  let val = src .&. src'
  registers.rn dest .= val