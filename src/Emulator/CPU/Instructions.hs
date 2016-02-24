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

-- Standard arithmetic add
add :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> m ()
add dest src1 src2 = do
  res1 <- use src1
  res2 <- use src2
  dest .= res1 + res2

-- Arithmetic add with carry
addc :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> m ()
addc dest src1 src2 = do
  res1 <- use src1
  res2 <- use src2
  dest .= res1 + res2
  when (checkCarry res1 res2) $ do
    cpsr.carry .= True
    dest += 1

-- Logical AND
and :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> m ()
and dest src1 src2 = do
  res1 <- use src1
  res2 <- use src2
  dest .= res1 .&. res2

-- Logical Exclusive Or
eor :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> m ()
eor dest src1 src2 = do
  res1 <- use src1
  res2 <- use src2
  dest .= res1 `xor` res2

checkCarry :: MWord -> MWord -> Bool
checkCarry a b = ((c .&. 0x00000000FFFFFFFF) `xor` c) /= 0
  where
    c :: DWord
    c = fromIntegral a + fromIntegral b
