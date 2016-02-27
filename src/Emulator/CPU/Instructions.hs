module Emulator.CPU.Instructions where

import Emulator.Types
import Emulator.CPU

import Control.Lens
import Control.Monad.State.Class
import Control.Monad
import Data.Bits
import Prelude hiding (Ordering(..))

type RegisterLabel = Lens' Registers MWord
type ConditionCode = Bool


-- Standard arithmetic add
add :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
add dest src1 src2 cCode = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 + res2
  dest .= val
  -- Update flags if the condition is true
  when cCode $ do
    cpsr.zero .= (val == 0)
    cpsr.sign .= checkSign val
    cpsr.overflow .= checkCarry res1 res2
    cpsr.carry .= checkCarry res1 res2


-- Arithmetic add with carry
adc :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
adc dest src1 src2 cCode = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 + res2
  dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    cpsr.zero .= (val == 0)
    cpsr.sign .= checkSign val
    cpsr.overflow .= (checkCarry res1 res2)
    when (checkCarry res1 res2) $ do
        cpsr.carry .= True
        dest += 1

-- Arithmetic subtract
sub :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
sub dest src1 src2 cCode = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 - res2
  dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    cpsr.zero .= (val == 0)
    cpsr.sign .= checkSign val
    -- TODO: Maybe detect this, but it's kind of contrived
    cpsr.overflow .= False
    cpsr.carry .= False

-- Subtract with carry
sbc :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
sbc dest src1 src2 cCode = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 - res2
  dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    cpsr.zero .= (val == 0)
    cpsr.sign .= checkSign val
    -- TODO: Maybe detect this, but it's kind of contrived
    cpsr.overflow .= False
    cpsr.carry .= False
    when (checkCarry res1 res2) $ do
        cpsr.carry .= True
        dest += 1

-- Subtract with carry reversed
rsc :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
rsc dest src1 src2 cCode = sbc dest src2 src1 cCode

-- Arithmetic subtract reversed
rsb :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
rsb dest src1 src2 cCode = sub dest src2 src1 cCode

-- Logical AND
and :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
and dest src1 src2 cCode = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 .&. res2
  dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    -- FIXME: This actually should be the carry flag from the shifted register
    -- IF the shifted register is used as an operand. Unfortunately we don't
    -- have shifted registers yet so this can stay false for now.
    cpsr.carry .= False
    cpsr.zero .= (val == 0)
    cpsr.sign .= checkSign val

-- Test instruction
tst :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
tst _ src1 src2 cCode = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 .&. res2
  -- Update flags if condition code is true
  when cCode $ do
    -- FIXME: This actually should be the carry flag from the shifted register
    -- IF the shifted register is used as an operand. Unfortunately we don't
    -- have shifted registers yet so this can stay false for now.
    cpsr.carry .= False
    cpsr.zero .= (val == 0)
    cpsr.sign .= checkSign val

-- Test exclusive (XOR)
teq :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
teq dest src1 src2 cCode = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 `xor` res2
  dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    -- FIXME: This actually should be the carry flag from the shifted register
    -- IF the shifted register is used as an operand. Unfortunately we don't
    -- have shifted registers yet so this can stay false for now.
    cpsr.carry .= False
    cpsr.zero .= (val == 0)
    cpsr.sign .= checkSign val


-- Compare
cmp :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
cmp dest src1 src2 cCode = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 - res2
  dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    cpsr.zero .= (val == 0)
    cpsr.sign .= checkSign val
    -- TODO: Maybe detect this, but it's kind of contrived
    cpsr.overflow .= False
    cpsr.carry .= False

-- Compare negative
cmn :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
cmn dest src1 src2 cCode = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 + res2
  dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    cpsr.zero .= (val == 0)
    cpsr.sign .= checkSign val
    -- TODO: Maybe detect this, but it's kind of contrived
    cpsr.overflow .= False
    cpsr.carry .= False

-- Logical Exclusive Or
eor :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
eor dest src1 src2 cCode = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 `xor` res2
  dest .= val
  -- Update flags if condition code is true
  when cCode $ do
    -- FIXME: see above
    cpsr.carry .= False
    cpsr.zero .= (val == 0)
    cpsr.sign .= checkSign val

-- Move instruction
mov :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> ConditionCode -> m ()
mov dest _ src2 _ = do
  res1 <- use src2
  dest .= res1

---------------------
-- Utils
---------------------

conditionally :: MonadState Registers m => Condition -> m a -> m ()
conditionally cond act = do
  res <- runCondition cond
  when res $ void act

runCondition :: MonadState Registers m => Condition -> m Bool
runCondition cond =
  case cond of
    EQ -> use (cpsr.zero)
    NE -> not <$> use (cpsr.zero)
    CS -> use (cpsr.carry)
    CC -> not <$> use (cpsr.carry)
    MI -> use (cpsr.sign)
    PL -> not <$> use (cpsr.sign)
    VS -> use (cpsr.overflow)
    VC -> not <$> use (cpsr.overflow)
    HI -> (&&) <$> use (cpsr.carry) <*> (not <$> use (cpsr.zero))
    LS -> (||) <$> (not <$> use (cpsr.carry)) <*> use (cpsr.zero)
    GE -> (==) <$> use (cpsr.sign) <*> use (cpsr.overflow)
    LT -> (/=) <$> use (cpsr.sign) <*> use (cpsr.overflow)
    GT ->
      (&&) <$> (not <$> use (cpsr.zero))
           <*> ((==) <$> use (cpsr.sign) <*> use (cpsr.overflow))
    LE ->
      (||) <$> (not <$> use (cpsr.zero))
           <*> ((/=) <$> use (cpsr.sign) <*> use (cpsr.overflow))
    AL -> return True

checkCarry :: MWord -> MWord -> Bool
checkCarry a b = ((c .&. 0x00000000FFFFFFFF) `xor` c) /= 0
  where
    c :: DWord
    c = fromIntegral a + fromIntegral b

checkSign :: MWord -> Bool
checkSign a = (a .&. 0x80000000) > 0
