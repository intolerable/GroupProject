module Emulator.Video.Palette where

import Emulator.Types
import Emulator.Video.Util
import Utilities.Parser.TemplateHaskell

import Data.Array.IArray

type Palette = Array Address Byte

pixelData :: PaletteFormat -> Palette -> PixData -> Address -> [HalfWord]
-- 256/1 palette format
pixelData True palette pixData _ = tilePixelDataList
  where
    tilePixelDataList = palette256 palette pixData (fst tileBounds) 64
    tileBounds = bounds pixData

-- 16/16 palette format
pixelData _ palette pixData palBank = tilePixelDataList
  where
    bank = ixmap (palBankAddr, (palBankAddr + 0x0000001F)) (id) palette :: Palette
    tilePixelDataList = palette16 bank pixData palBankAddr (fst tileBounds) 32
    tileBounds = bounds pixData
    palLowBound = fst $ bounds palette
    palBankAddr = palLowBound + palBank

palette16 :: Palette -> PixData -> Address -> Address -> Int -> [HalfWord]
palette16 _ _ _ _ 0 = []
palette16 bank pixData palBankBaseAddr pixDataAddr n = col1:col2:palette16 bank pixData palBankBaseAddr (pixDataAddr + 0x00000001) (n-1)
  where
    byt = pixData!pixDataAddr
    nib1 = 2 * (fromIntegral $ $(bitmask 3 0) byt :: Address)
    nib2 = 2 * (fromIntegral $ $(bitmask 7 4) byt :: Address)
    col1Byt1 = bank!(nib1 + palBankBaseAddr)
    col1Byt2 = bank!(nib1 + palBankBaseAddr + 0x00000001)
    col1 = if testTransparency nib1 then 0B1000000000000000 else bytesToHalfWord col1Byt1 col1Byt2
    col2Byt1 = bank!(nib2 + palBankBaseAddr)
    col2Byt2 = bank!(nib2 + palBankBaseAddr + 0x00000001)
    col2 = if testTransparency nib2 then 0B1000000000000000 else bytesToHalfWord col2Byt1 col2Byt2

palette256 :: Palette -> PixData -> Address -> Int -> [HalfWord]
palette256 _ _ _ 0 = []
palette256 palette pixData pixDataAddr n = col:palette256 palette pixData (pixDataAddr + 0x00000001) (n-1)
  where
    palLowBound = fst $ bounds palette
    addr = palLowBound + 2 * (fromIntegral $ pixData!pixDataAddr :: Address)
    colByt1 = palette!addr
    colByt2 = palette!(addr + 0x00000001)
    col = if testTransparency addr then 0B1000000000000000 else bytesToHalfWord colByt1 colByt2

testTransparency :: Address -> Bool
testTransparency indx
  | indexVal == 0 = True
  | otherwise = False
  where
    indexVal = (fromIntegral $ $(bitmask 7 0) indx) :: Int
