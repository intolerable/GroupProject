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

bitmapModes :: AddressIO m => LCDControl -> m (ScreenObj)
bitmapModes cnt = do
  bgCNT <- recordBGControl 0x0400000C
  xWord <- readAddressWord 0x04000028
  yWord <- readAddressWord 0x0400002C
  paramMem <- readRange (0x04000020, 0x04000027)
  vram <- readBitmapVram (bgMode cnt) (displayFrameSelect cnt)
  palette <- readRange (0x05000000, 0x050001FF)
  let refPoint = (referencePoint xWord, referencePoint yWord)
  let params = affineParameters 0x04000020 0x04000022 0x04000024 0x04000026 paramMem
  case bgMode cnt of
    3 -> return (mode3n5 bgCNT refPoint params (240, 160) vram)
    4 -> return (mode4 bgCNT refPoint params (240, 160) vram palette)
    _ -> return (mode3n5 bgCNT refPoint params (160, 128) vram)

readBitmapVram :: AddressSpace m => Byte -> Bool -> m (Array Address Byte)
readBitmapVram 3 _ = readRange (0x06000000, 0x06012BFF)
readBitmapVram 4 True = readRange (0x0600A000, 0x060135FF)
readBitmapVram 4 False = readRange (0x06000000, 0x0060095FF)
readBitmapVram _ True = readRange (0x0600A000, 0x006013FFF)
readBitmapVram _ False = readRange (0x06000000, 0x006009FFF)

mode3n5 :: BGControl -> AffineRefPoints -> AffineParameters -> (Int, Int) -> Array Address Byte -> ScreenObj
mode3n5 bgCNT (x, y) params (w, h) vram = BitmapBG quad (bgPriority bgCNT)
  where
    bitmapList = convToBitmap vram (fst (bounds vram)) (w * h)
    centre = (x + fromIntegral w, y + fromIntegral h)
    preAffineCoords = ((x, y), (x+8, y), (x, y+8), (x+8, y+8))
    coords = affineCoords centre params preAffineCoords
    quad = Tile bitmapList coords

mode4 :: BGControl -> AffineRefPoints -> AffineParameters -> (Int, Int) -> Array Address Byte -> Palette -> ScreenObj
mode4 bgCNT (x, y) params (w, h) vram pal = BitmapBG quad (bgPriority bgCNT)
  where
    bitmapList = palette256 pal vram (fst (bounds vram)) (w * h)
    centre = (x + fromIntegral w, y + fromIntegral h)
    preAffineCoords = ((x, y), (x+8, y), (x, y+8), (x+8, y+8))
    coords = affineCoords centre params preAffineCoords
    quad = Tile bitmapList coords

convToBitmap :: Array Address Byte -> Address -> Int -> [HalfWord]
convToBitmap _ _ 0 = []
convToBitmap vram baseAddr nPixels = col:convToBitmap vram (baseAddr + 0x00000002) (nPixels - 1)
  where
    colByte1 = vram!baseAddr
    colByte2 = vram!(baseAddr + 0x00000001)
    col = bytesToHalfWord colByte1 colByte2
