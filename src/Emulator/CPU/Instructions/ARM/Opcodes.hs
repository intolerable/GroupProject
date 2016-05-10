module Emulator.CPU.Instructions.ARM.Opcodes
  ( functionFromOpcode
  , SrcRegister
  , DestRegister
  , ShiftRegister
  , ConditionCode
  , and
  , eor
  , sub
  , rsb
  , add
  , adc
  , sbc
  , rsc
  , tst
  , teq
  , cmp
  , cmn
  , orr
  , mov
  , bic
  , mvn )
  where

import Emulator.Types
import Emulator.CPU
import Emulator.CPU.Instructions.Flags

import Control.Lens
import Control.Monad.State.Class
import Control.Monad
import Data.Bits
import Data.Int (Int32)
import Prelude hiding (Ordering(..), and)

type SrcRegister = Getting MWord Registers MWord
type DestRegister = ASetter' Registers MWord
type ShiftRegister = Getting (Bool, MWord) Registers (Bool, MWord)
type ConditionCode = Bool

functionFromOpcode :: (HasFlags s, HasRegisters s, MonadState s m)
                   => Opcode -> DestRegister -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
functionFromOpcode opcode =
  case opcode of
    AND -> and
    EOR -> eor
    SUB -> sub
    RSB -> rsb
    ADD -> add
    ADC -> adc
    SBC -> sbc
    RSC -> rsc
    TST -> tst
    TEQ -> teq
    CMP -> cmp
    CMN -> cmn
    ORR -> orr
    MOV -> mov
    BIC -> bic
    MVN -> mvn

---------------------
-- Data processing instructions
---------------------

-- Standard arithmetic add
add :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
add dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  (_, res2) <- use $ registers.src2
  let val = res1 + res2
  registers.dest .= val
  -- Update flags if the condition is true
  when cCode $ do
    flags.negative .= arithmeticNegative val
    flags.zero .= arithmeticZero val
    flags.carry .= arithmeticCarry (+) res1 res2
    flags.overflow .= arithmeticAddOverflow res1 res2 val

-- Arithmetic add with carry
adc :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
adc dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  (isCarry, res2) <- use $ registers.src2
  let cy = if isCarry then 1 else 0
  let val = res1 + res2 + cy
  registers.dest .= val
  when cCode $ do
    setFlagsLogic val
    flags.carry .= arithmeticCarry (+) res1 (res2 + cy)
    flags.overflow .= arithmeticAddOverflow res1 (res2 + cy) val

-- Arithmetic subtract
sub :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
sub dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  (_, res2) <- use $ registers.src2
  let val = res1 - res2
  registers.dest .= val
  when cCode $ do
    setFlagsLogic val
    flags.carry .= ((res1 > (val - res2)) || (res1 == 0 && res2 == 0))
    flags.overflow .= False

-- Arithmetic subtract reversed
rsb :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
rsb dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  (_, res2) <- use $ registers.src2
  let val = res2 - res1
  registers.dest .= val
  when cCode $ do
    setFlagsLogic val
    flags.carry .= ((res2 > (val - res1)) || (res1 == 0 && res2 == 0))
    flags.overflow .= False

-- Subtract with carry
sbc :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
sbc dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  (isCarry, res2) <- use $ registers.src2
  let cy = if isCarry then 1 else 0
  let val = ((res1 - res2) - 1) + cy
  registers.dest .= val
  when cCode $ do
    setFlagsLogic val
    flags.carry .= ((res1 > (val - res2)) || (res1 == 0 && res2 == 0))
    flags.overflow .= False

-- Subtract with carry reversed
rsc :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
rsc dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  (isCarry, res2) <- use $ registers.src2
  let cy = if isCarry then 1 else 0
  let val = ((res2 - res1) - 1) + cy
  registers.dest .= val
  when cCode $ do
    setFlagsLogic val
    flags.carry .= ((res2 > (val - res1)) || (res1 == 0 && res2 == 0))
    flags.overflow .= False

-- Logical/bitwise AND
and :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
and dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  (cy, res2) <- use $ registers.src2
  let val = res1 .&. res2
  registers.dest .= val
  when cCode $ do
    setFlagsLogic val
    flags.carry .= cy

-- Logical/bitwise OR
orr :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
orr dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  (cy, res2) <- use $ registers.src2
  let val = res1 .|. res2
  registers.dest .= val
  when cCode $ do
    setFlagsLogic val
    flags.carry .= cy

-- Move Negative (aka Move NOT)
mvn :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> a -> ShiftRegister -> ConditionCode -> m ()
mvn dest _ src2 cCode = do
  (cy, res2) <- use $ registers.src2
  let val = complement res2
  registers.dest .= val
  when cCode $ do
    setFlagsLogic val
    flags.carry .= cy

-- Bit clear
bic :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
bic dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  (cy, res2) <- use $ registers.src2
  let val = res1 .&. (complement res2)
  registers.dest .= val
  when cCode $ do
    setFlagsLogic val
    flags.carry .= cy

-- Test instruction
tst :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
tst _ src1 src2 True = do
  res1 <- use $ registers.src1
  (isCarry, res2) <- use $ registers.src2
  let val = res1 .&. res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  setFlagsLogic val
  flags.carry .= isCarry
tst _ _ _ False = return () -- MSR/MRS stuff

-- Test exclusive (XOR)
teq :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
teq _ src1 src2 True = do
  res1 <- use $ registers.src1
  (cy, res2) <- use $ registers.src2
  let val = res1 `xor` res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  setFlagsLogic val
  flags.carry .= cy
teq _ _ _ False = return () -- MSR/MRS stuff

-- Compare
cmp :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
cmp _ src1 src2 True = do
  res1 <- use $ registers.src1
  (isCarried, res2) <- use $ registers.src2
  let val = res1 - res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  setFlagsLogic val
  flags.carry .= ((res1 > (val - res2)) || (res1 == 0 && res2 == 0))
  flags.overflow .= False
cmp _ _ _ False = return () -- MSR/MRS stuff

-- Compare negative
cmn :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
cmn _ src1 src2 True = do
  res1 <- use $ registers.src1
  (isCarried, res2) <- use $ registers.src2
  let val = res1 + res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  setFlagsLogic val
  flags.carry .= arithmeticCarry (+) res1 res2
  flags.overflow .= arithmeticAddOverflow res1 res2 val
cmn _ _ _ False = return () -- MSR/MRS stuff

-- Logical Exclusive Or
eor :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> ShiftRegister -> ConditionCode -> m ()
eor dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  (cy, res2) <- use $ registers.src2
  let val = res1 `xor` res2
  registers.dest .= val
  when cCode $ do
    setFlagsLogic val
    flags.carry .= cy

-- Move instruction
mov :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> a -> ShiftRegister -> ConditionCode -> m ()
mov dest _ src2 cCode = do
  (cy, val) <- use $ registers.src2
  registers.dest .= val
  when cCode $ do
    setFlagsLogic val
    flags.carry .= cy
