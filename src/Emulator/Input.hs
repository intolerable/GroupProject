module Emulator.Input where

import Emulator.Types

import Data.Bits

data Input = ButtonA
           | ButtonB
           | ButtonSelect
           | ButtonStart
           | ButtonRight
           | ButtonLeft
           | ButtonUp
           | ButtonDown
           | ButtonR
           | ButtonL
  deriving (Show, Enum)

inputBitmask :: [Input] -> HalfWord
inputBitmask inputs = inputBitmask' 0 inputs
  where
    inputBitmask' :: HalfWord -> [Input] -> HalfWord
    inputBitmask' = foldl (\ w' i -> (setBit (fromIntegral w') (bitIndexFromInput i)))

bitIndexFromInput :: Input -> Int
bitIndexFromInput w = fromIntegral $ fromEnum w
