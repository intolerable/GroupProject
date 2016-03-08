module Emulator.CPU.Instructions where

import Emulator.Types
import Emulator.CPU

import Control.Lens
import Control.Monad.State.Class
import Control.Monad
import Data.Bits
import Prelude hiding (Ordering(..))

type RegisterLabel = Lens' Registers MWord
type SrcRegister = Getting MWord Registers MWord
type DestRegister = ASetter' Registers MWord
type ConditionCode = Bool

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
    flags.sign .= checkSign val
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
    flags.sign .= checkSign val
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
    flags.sign .= checkSign val
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
    flags.sign .= checkSign val
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
    flags.sign .= checkSign val

-- Test instruction
tst :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
tst _ src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 .&. res2
  -- Update flags if condition code is true
  when cCode $ do
    -- FIXME: This actually should be the carry flag from the shifted register
    -- IF the shifted register is used as an operand. Unfortunately we don't
    -- have shifted registers yet so this can stay false for now.
    flags.carry .= False
    flags.zero .= (val == 0)
    flags.sign .= checkSign val

-- Test exclusive (XOR)
teq :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
teq _ src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 `xor` res2
  -- Update flags if condition code is true
  when cCode $ do
    -- FIXME: This actually should be the carry flag from the shifted register
    -- IF the shifted register is used as an operand. Unfortunately we don't
    -- have shifted registers yet so this can stay false for now.
    flags.carry .= False
    flags.zero .= (val == 0)
    flags.sign .= checkSign val

-- Compare
cmp :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
cmp _ src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 - res2
  -- Update flags if condition code is true
  when cCode $ do
    flags.zero .= (val == 0)
    flags.sign .= checkSign val
    -- TODO: Maybe detect this, but it's kind of contrived
    flags.overflow .= False
    flags.carry .= False

-- Compare negative
cmn :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
cmn _ src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 + res2
  -- Update flags if condition code is true
  when cCode $ do
    flags.zero .= (val == 0)
    flags.sign .= checkSign val
    -- TODO: Maybe detect this, but it's kind of contrived
    flags.overflow .= False
    flags.carry .= False

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
    flags.sign .= checkSign val

-- Move instruction
mov :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
mov dest _ src2 _ = do
  res1 <- use $ registers.src2
  registers.dest .= res1

---------------------
-- Utils
---------------------

conditionally :: (HasFlags s, MonadState s m) => Condition -> m a -> m ()
conditionally cond act = do
  res <- runCondition cond
  when res $ void act

runCondition :: (HasFlags s, MonadState s m) => Condition -> m Bool
runCondition cond =
  case cond of
    EQ -> use (flags.zero)
    NE -> not <$> use (flags.zero)
    CS -> use (flags.carry)
    CC -> not <$> use (flags.carry)
    MI -> use (flags.sign)
    PL -> not <$> use (flags.sign)
    VS -> use (flags.overflow)
    VC -> not <$> use (flags.overflow)
    HI -> (&&) <$> use (flags.carry) <*> (not <$> use (flags.zero))
    LS -> (||) <$> (not <$> use (flags.carry)) <*> use (flags.zero)
    GE -> (==) <$> use (flags.sign) <*> use (flags.overflow)
    LT -> (/=) <$> use (flags.sign) <*> use (flags.overflow)
    GT ->
      (&&) <$> (not <$> use (flags.zero))
           <*> ((==) <$> use (flags.sign) <*> use (flags.overflow))
    LE ->
      (||) <$> (not <$> use (flags.zero))
           <*> ((/=) <$> use (flags.sign) <*> use (flags.overflow))
    AL -> return True

checkCarry :: MWord -> MWord -> Bool
checkCarry a b = ((c .&. 0x00000000FFFFFFFF) `xor` c) /= 0
  where
    c :: DWord
    c = fromIntegral a + fromIntegral b

checkSign :: MWord -> Bool
checkSign a = (a .&. 0x80000000) > 0
