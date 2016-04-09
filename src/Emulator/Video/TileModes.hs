module Emulator.Video.TileModes where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Util
import Emulator.Video.VideoController

import Graphics.Rendering.OpenGL

tileModes :: AddressSpace m => LCDControl -> m ()
tileModes cnt = do
  case bgMode cnt of
    0 -> mode0 cnt
    _ -> undefined

mode0 :: AddressSpace m => LCDControl -> m ()
mode0 = do
  return bg0

-- Text Mode
bg0 :: AddressSpace m => m ()
bg0 = do
  bg <- recordBGControl 0x04000008
  bgOffset <- recordBGOffset 0x04000010 0x04000020
  let tileBase = characterBaseBlock bg
  let tileData = getTileBlock tileBase
  return ()

drawVLines :: String -> Int -> Int -> (GLdouble, GLdouble) -> Address -> IO ()
drawVLines _ 0 _ _ _ = return ()
drawVLines fname n columns (x, y) addr = do
  drawHLine fname columns (x, y) addr
  drawVLines fname (n-1) columns (x, y+8) addr

drawHLine :: String -> Int -> (GLdouble, GLdouble) -> Address -> IO ()
drawHLine _ (0) _ _ = return ()
drawHLine fname n (x, y) addr = do
  _ <- drawTile fname (x, x+8) (y, y+8)
  drawHLine fname (n-1) (x+8,y) (addr + 0x00000020)

getTileBlock :: Byte -> Address
getTileBlock tileBase
  | tileBase == 0 = 0x06000000
  | tileBase == 1 = 0x06004000
  | tileBase == 2 = 0x06008000
  | otherwise = 0x0600C000

-- Returns number of tiles to be drawn
textBGSize :: Byte -> (Int, Int)
textBGSize byt
  | byt == 0 = (32, 32)
  | byt == 1 = (64, 32)
  | byt == 2 = (32, 64)
  | otherwise = (64, 32)

-- Returns number of tiles to be drawn
affineBGSize :: Byte -> (Int, Int)
affineBGSize byt
  | byt == 0 = (16, 16)
  | byt == 1 = (32, 32)
  | byt == 2 = (64, 64)
  | otherwise = (128, 128)
