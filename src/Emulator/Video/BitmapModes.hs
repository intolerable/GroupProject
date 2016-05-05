module Emulator.Video.BitmapModes where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Util
import Emulator.Video.Palette
import Emulator.Video.Renderer
import Emulator.Video.VideoController

import Control.Monad.IO.Class
import Data.Array.IArray
import Data.Array.Storable

type Bitmap = Array Address Byte

bitmapModes :: AddressIO m => LCDControl -> m ()
bitmapModes cnt = do
  bgCNT <- recordBGControl 0x0400000C
  xWord <- readAddressWord 0x04000028
  yWord <- readAddressWord 0x0400002C
  paramMem <- readRange (0x04000020, 0x04000027)
  let refPoint = (referencePoint xWord, referencePoint yWord)
  let params = affineParameters 0x04000020 0x04000022 0x04000024 0x04000026 paramMem
  case bgMode cnt of
    3 -> mode3n5 bgCNT refPoint params (240, 160) (0x06000000, 0x06012BFF)
    4 -> if displayFrameSelect cnt
      then mode4 bgCNT refPoint params (240, 160) (0x0600A000, 0x060135FF)
      else mode4 bgCNT refPoint params (240, 160) (0x06000000, 0x0060095FF)
    _ -> if displayFrameSelect cnt
      then mode3n5 bgCNT refPoint params (160, 128) (0x0600A000, 0x006013FFF)
      else mode3n5 bgCNT refPoint params (160, 128) (0x06000000, 0x006009FFF)
  return ()

mode3n5 :: AddressIO m => BGControl -> AffineRefPoints -> AffineParameters -> (Int, Int) -> (Address, Address) -> m ()
mode3n5 _bgCNT ref@(x, y) params (w, h) vramAddr = do
  vram <- readRange vramAddr
  let bitmapList = convToBitmap vram (fst vramAddr) (w * h)
  bitmapArray <- liftIO $ newListArray vramAddr bitmapList
  let centre = (x + fromIntegral w, y + fromIntegral h)
  let coords = affineCoords ref centre params
  liftIO $ drawTile bitmapArray coords
  return ()

mode4 :: AddressIO m => BGControl -> AffineRefPoints -> AffineParameters -> (Int, Int) -> (Address, Address) -> m ()
mode4 _bgCNT ref@(x, y) params (w, h) vramAddr = do
  vram <- readRange vramAddr
  pal <- readRange (0x05000000, 0x050001FF)
  let bitmapList = palette256 pal vram (fst vramAddr) (w * h)
  bitmapArray <- liftIO $ newListArray vramAddr bitmapList
  let centre = (x + fromIntegral w, y + fromIntegral h)
  let coords = affineCoords ref centre params
  liftIO $ drawTile bitmapArray coords
  return ()

convToBitmap :: Array Address Byte -> Address -> Int -> [HalfWord]
convToBitmap _ _ 0 = []
convToBitmap vram baseAddr nPixels = col:convToBitmap vram (baseAddr + 0x00000002) (nPixels - 1)
  where
    colByte1 = vram!baseAddr
    colByte2 = vram!(baseAddr + 0x00000001)
    col = bytesToHalfWord colByte1 colByte2
