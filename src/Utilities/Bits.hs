module Utilities.Bits where

import Emulator.Types

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

-- For when we cant use the TemplateHaskell version
staticBitmask :: Int -> Int -> MWord -> MWord
staticBitmask x y w = (w .&. mask) `shiftR` y
  where
    a, b :: MWord
    a = 1 `shiftL` fromIntegral x
    b = 1 `shiftL` fromIntegral y
    mask :: MWord
    mask = ((a - 1) .&. complement (b - 1)) .|. a
