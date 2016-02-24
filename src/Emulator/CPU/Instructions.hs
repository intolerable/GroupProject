module Emulator.CPU.Instructions where

import Emulator.Types
import Emulator.CPU

import Control.Lens
import Control.Monad.State.Class

type RegisterLabel = Lens' Registers MWord

add :: MonadState Registers m => RegisterLabel -> RegisterLabel -> RegisterLabel -> m ()
add dest src1 src2 = do
  res1 <- use src1
  res2 <- use src2
  dest .= res1 + res2
checkCarry :: MWord -> MWord -> Bool
checkCarry a b =
  popCount ((fromIntegral a + fromIntegral b) .&. (0xFFFFFFFF00000000 :: DWord)) > 0
