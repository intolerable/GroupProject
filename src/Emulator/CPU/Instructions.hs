module Emulator.CPU.Instructions where

import Emulator.Types
import Emulator.CPU

import Control.Applicative
import Control.Lens
import Control.Monad.State.Class
import Control.Monad
import Data.Bits
import Prelude hiding (Ordering(..))

type RegisterLabel = Lens' Registers MWord


-- Standard arithmetic add
add :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> m ()
add dest src1 src2 = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 + res2
  dest .= val
  -- Update flags
  cpsr.zero .= (val == 0)
  cpsr.sign .= checkSign val
  cpsr.overflow .= checkCarry res1 res2
  cpsr.carry .= checkCarry res1 res2


-- Arithmetic add with carry
adc :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> m ()
adc dest src1 src2 = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 + res2
  dest .= val
  -- Update flags
  cpsr.zero .= (val == 0)
  cpsr.sign .= checkSign val
  cpsr.overflow .= (checkCarry res1 res2)
  when (checkCarry res1 res2) $ do
    cpsr.carry .= True
    dest += 1

-- Arithmetic subtract
sub :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> m ()
sub dest src1 src2 = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 - res2
  dest .= val
  -- Update flags
  cpsr.zero .= (val == 0)
  cpsr.sign .= checkSign val
  -- TODO: Maybe detect this, but it's kind of contrived
  cpsr.overflow .= False
  cpsr.carry .= False

-- Arithmetic subtract reversed
rsb :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> m ()
rsb dest src1 src2 = sub dest src2 src1

-- Logical AND
and :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> m ()
and dest src1 src2 = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 .&. res2
  dest .= val
  -- Update flags
  -- FIXME: This actually should be the carry flag from the shifted register
  -- IF the shifted register is used as an operand. Unfortunately we don't
  -- have shifted registers yet so this can stay false for now.
  cpsr.carry .= False
  cpsr.zero .= (val == 0)
  cpsr.sign .= checkSign val

-- Logical Exclusive Or
eor :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> m ()
eor dest src1 src2 = do
  res1 <- use src1
  res2 <- use src2
  let val = res1 `xor` res2
  dest .= val
  -- Update flags
  -- FIXME: see above
  cpsr.carry .= False
  cpsr.zero .= (val == 0)
  cpsr.sign .= checkSign val


---------------------
-- Utils
---------------------

conditionally :: (MonadState Registers m, Applicative m)
              => Condition -> m a -> m ()
conditionally cond act = do
  res <- runCondition cond
  when res $ void act

runCondition :: (MonadState Registers m, Applicative m)
             => Condition -> m Bool
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
