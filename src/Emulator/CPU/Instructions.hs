module Emulator.CPU.Instructions where

import Emulator.Types
import Emulator.CPU

import Control.Lens
import Control.Monad.State.Class
import Control.Monad
import Data.Bits

type RegisterLabel = Lens' Registers MWord


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


checkCarry :: MWord -> MWord -> Bool
checkCarry a b =
  popCount ((fromIntegral a + fromIntegral b) .&. (0xFFFFFFFF00000000 :: DWord)) > 0
