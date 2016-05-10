module Emulator.Video.Palette where

import Emulator.Types
import Emulator.Video.Util
import Utilities.Parser.TemplateHaskell

import Data.Array.IArray
import Data.Bits

type Palette = Array Address Byte

pixelData :: PixFormat -> Palette -> TilePixData -> Address -> [HalfWord]
-- 256/1 palette format
pixelData True palette tile _ = tilePixelDataList
  where
    tilePixelDataList = palette256 palette tile (fst tileBounds) 64
    tileBounds = bounds tile

-- 16/16 palette format
pixelData _ palette tile palBank = tilePixelDataList
  where
    bank = ixmap (palBankAddr, (palBankAddr + 0x0000001F)) (id) palette :: Palette
    tilePixelDataList = palette16 bank tile palBankAddr (fst tileBounds) 32
    tileBounds = bounds tile
    palLowBound = fst $ bounds palette
    palBankAddr = palLowBound + palBank

palette16 :: Palette -> TilePixData -> Address -> Address -> Int -> [HalfWord]
palette16 _ _ _ _ 0 = []
palette16 bank tile palBankBaseAddr tileAddr n = col1:col2:palette16 bank tile palBankBaseAddr (tileAddr + 0x00000001) (n-1)
  where
    byt = tile!tileAddr
    nib1 = 2 * (fromIntegral $ $(bitmask 3 0) byt :: Address)
    nib2 = 2 * (fromIntegral $ $(bitmask 7 4) byt :: Address)
    col1Byt1 = bank!(nib1 + palBankBaseAddr)
    col1Byt2 = bank!(nib1 + palBankBaseAddr + 0x00000001)
    col1 = testTransparency $ bytesToHalfWord col1Byt1 col1Byt2
    col2Byt1 = bank!(nib2 + palBankBaseAddr)
    col2Byt2 = bank!(nib2 + palBankBaseAddr + 0x00000001)
    col2 = testTransparency $ bytesToHalfWord col2Byt1 col2Byt2

palette256 :: Palette -> TilePixData -> Address -> Int -> [HalfWord]
palette256 _ _ _ 0 = []
palette256 palette tile tileAddr n = col:palette256 palette tile (tileAddr + 0x00000001) (n-1)
  where
    palLowBound = fst $ bounds palette
    addr = palLowBound + 2 * (fromIntegral $ tile!tileAddr :: Address)
    colByt1 = palette!addr
    colByt2 = palette!(addr + 0x00000001)
    col = testTransparency $ bytesToHalfWord colByt1 colByt2

testTransparency :: HalfWord -> HalfWord
testTransparency pixData
  | pixValue == 0 = pixData `setBit` 15
  | otherwise = pixData `clearBit` 15
  where
    pixValue = (fromIntegral $ $(bitmask 14 0) pixData) :: Int
