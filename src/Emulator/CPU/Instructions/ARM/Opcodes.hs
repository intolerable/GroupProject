module Emulator.CPU.Instructions.ARM.Opcodes where

import Emulator.Types
import Emulator.CPU

import Control.Lens
import Control.Monad.State.Class
import Control.Monad
import Data.Bits
import Prelude hiding (Ordering(..), and)

type RegisterLabel = Lens' Registers MWord
type SrcRegister = Getting MWord Registers MWord
type DestRegister = ASetter' Registers MWord
type ConditionCode = Bool

functionFromOpcode :: (HasFlags s, HasRegisters s, MonadState s m)
                   => Opcode -> DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
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
-- TODO: almost all flags need checking/fixing
---------------------

-- Standard arithmetic add
add :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
add dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 + res2
  registers.dest .= val
  -- Update flags if the condition is true
  when cCode $ do
    flags.zero .= (val == 0)
    flags.negative .= isNegative val
    flags.overflow .= checkCarry res1 res2
    flags.carry .= checkCarry res1 res2

-- Arithmetic add with carry
adc :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
adc dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 + res2
  registers.dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    flags.zero .= (val == 0)
    flags.negative .= isNegative val
    flags.overflow .= (checkCarry res1 res2)
    when (checkCarry res1 res2) $ do
        flags.carry .= True
        registers.dest .= 1

-- Arithmetic subtract
sub :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
sub dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 - res2
  registers.dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    flags.zero .= (val == 0)
    flags.negative .= isNegative val
    -- TODO: Maybe detect this, but it's kind of contrived
    flags.overflow .= False
    flags.carry .= False

-- Subtract with carry
sbc :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
sbc dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 - res2
  registers.dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    flags.zero .= (val == 0)
    flags.negative .= isNegative val
    -- TODO: Maybe detect this, but it's kind of contrived
    flags.overflow .= False
    flags.carry .= False
    when (checkCarry res1 res2) $ do
        flags.carry .= True
        registers.dest .= 1

-- Subtract with carry reversed
rsc :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
rsc dest src1 src2 cCode = sbc dest src2 src1 cCode

-- Arithmetic subtract reversed
rsb :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
rsb dest src1 src2 cCode = sub dest src2 src1 cCode

-- Logical AND
and :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
and dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 .&. res2
  registers.dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    -- FIXME: This actually should be the carry flag from the shifted register
    -- IF the shifted register is used as an operand. Unfortunately we don't
    -- have shifted registers yet so this can stay false for now.
    flags.carry .= False
    flags.zero .= (val == 0)
    flags.negative .= isNegative val

orr :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
orr dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 .|. res2
  registers.dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    -- used and flags, TODO
    flags.carry .= False
    flags.zero .= (val == 0)
    flags.negative .= isNegative val

-- not
mvn :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
mvn dest _ src2 cCode = do
  res2 <- use $ registers.src2
  let val = complement res2
  registers.dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    -- used and flags, TODO
    flags.carry .= False
    flags.zero .= (val == 0)
    flags.negative .= isNegative val

-- Bit clear
bic :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
bic dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 .&. (complement res2)
  registers.dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    -- used and flags, TODO
    flags.carry .= False
    flags.zero .= (val == 0)
    flags.negative .= isNegative val


-- Test instruction
tst :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
tst _ src1 src2 _ = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 .&. res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  flags.negative .= isNegative val
  flags.zero .= (val == 0)
  -- TODO: carry from barrel shifter
  flags.carry .= False
  -- overflow is untouched

-- Test exclusive (XOR)
teq :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
teq _ src1 src2 _ = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 `xor` res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  flags.negative .= isNegative val
  flags.zero .= (val == 0)
  -- TODO: carry from barrel shifter
  flags.carry .= False
  -- overflow is untouched

-- Compare
cmp :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
cmp _ src1 src2 _ = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 - res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  flags.zero .= (val == 0)
  flags.negative .= isNegative val
  flags.carry .= False
  flags.overflow .= isOverflow (-) res1 res2 val

-- Compare negative
cmn :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
cmn _ src1 src2 _ = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 + res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  flags.zero .= (val == 0)
  flags.negative .= isNegative val
  flags.carry .= False
  flags.overflow .= isOverflow (+) res1 res2 val

-- Logical Exclusive Or
eor :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
eor dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 `xor` res2
  registers.dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    -- FIXME: see above
    flags.carry .= False
    flags.zero .= (val == 0)
    flags.negative .= isNegative val

-- Move instruction
mov :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
mov dest _ src2 _ = do
  res1 <- use $ registers.src2
  registers.dest .= res1

checkCarry :: MWord -> MWord -> Bool
checkCarry a b = ((c .&. 0x00000000FFFFFFFF) `xor` c) /= 0
  where
    c :: DWord
    c = fromIntegral a + fromIntegral b

isNegative :: MWord -> Bool
isNegative a = (a .&. 0x80000000) > 0

isOverflow :: (a ~ Integer) => (a -> a -> a) -> MWord -> MWord -> MWord -> Bool
isOverflow f arg1 arg2 result = (fromIntegral result) /= preciseResult
  where
    preciseResult = ((fromIntegral arg1) `f` (fromIntegral arg2)) :: Integer
