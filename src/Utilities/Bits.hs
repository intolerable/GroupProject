module Utilities.Bits where

import Emulator.Types
import Utilities.Parser.TemplateHaskell

import Data.Bits
import Data.Int

-- This is what the halfWordExtend function is doing but we've opted for
-- the fromIntegral approach which has a chance to be faster (untested)
--halfWordExtend :: HalfWord -> MWord
--halfWordExtend h = val .|. orVal
--  where
--    val = fromIntegral (h .&. 0xFFFF) :: MWord
--    orVal = case testBit h 15 of
--      True  -> 0xFFFF0000
--      False -> 0x00000000
halfWordExtend :: HalfWord -> MWord
halfWordExtend h = fromIntegral (fromIntegral (fromIntegral h :: Int16) :: Int32)

byteExtend :: Byte -> MWord
byteExtend b = fromIntegral (fromIntegral (fromIntegral b :: Int8) :: Int32)

arithExtend :: MWord -> Int -> MWord
arithExtend v n = clearBit (if (testBit v n) then setBit v 31 else v) n

twosCompExtend :: MWord -> Int -> MWord
twosCompExtend w n = w .|. mask
  where
    mask | testBit w n = $(bitmask 31 n) 0xFFFFFFFF
         | otherwise = 0
