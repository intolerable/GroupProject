module Emulator.Video.Palette where

import Emulator.Types
import Emulator.Video.Util
import Utilities.Parser.TemplateHaskell

import Control.Monad.IO.Class
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.Storable
import Data.Bits
import Graphics.Rendering.OpenGL

pixelData :: AddressIO m => PixFormat -> Palette -> Tile -> Address -> m (StorableArray Address HalfWord)
-- 256/1 palette format
pixelData True palette tile _ = do
  let tilePixelDataList = palette256 palette tile (fst tileBounds) 0
  tilePixelData <- liftIO $ newListArray tileBounds tilePixelDataList
  return tilePixelData
  where
    tileBounds = bounds tile
-- 16/16 palette format
pixelData _ palette tile palBank = do
  let bank = ixmap (palBankAddr, (palBankAddr + 0x0000001F)) (id) palette :: Palette
  let tilePixelDataList = palette16 bank tile palBankAddr (fst tileBounds) 0
  tilePixelData <- liftIO $ newListArray tileBounds tilePixelDataList
  return tilePixelData
  where
    tileBounds = bounds tile
    palBankAddr = 0x05000000 + palBank

palette16 :: Palette -> Tile -> Address -> Address -> Int -> [HalfWord]
palette16 _ _ _ _ 32 = []
palette16 bank tile palBankBaseAddr tileAddr n = col1:col2:palette16 bank tile palBankBaseAddr tileAddr (n+1)
  where
    byt = tile!(tileAddr + (0x00000001 * fromIntegral n))
    nib1 = fromIntegral $ $(bitmask 3 0) byt :: Address
    nib2 = fromIntegral $ $(bitmask 7 4) byt :: Address
    col1Byt1 = bank!(nib1 + palBankBaseAddr)
    col1Byt2 = bank!(nib1 + palBankBaseAddr + 0x00000001)
    col1 = bytesToHalfWord col1Byt1 col1Byt2
    col2Byt1 = bank!(nib2 + palBankBaseAddr)
    col2Byt2 = bank!(nib2 + palBankBaseAddr + 0x00000001)
    col2 = bytesToHalfWord col2Byt1 col2Byt2

palette256 :: Palette -> Tile -> Address -> Int -> [HalfWord]
palette256 _ _ _ 64 = []
palette256 palette tile tileAddr n = col:palette256 palette tile tileAddr (n+1)
  where
    addr = 0x05000000 + (fromIntegral $ tile!(tileAddr + (0x00000001 * fromIntegral n)) :: Address)
    colByt1 = palette!addr
    colByt2 = palette!(addr + 0x00000001)
    col = bytesToHalfWord colByt1 colByt2
