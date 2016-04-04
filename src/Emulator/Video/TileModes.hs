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

bg0 :: AddressSpace m => m ()
bg0 = do
  bg <- recordBGControl 0x04000008
  let tileBaseBlock = characterBaseBlock bg
  let tileData = tileBaseBlock + 0x06000000
  -- tileMemOffset <- characterBaseBlock bg
  return ()

drawRow :: Int -> (GLdouble, GLdouble) -> Address -> IO ()
drawRow (-1) _ _ = return ()
drawRow n (x, y) addr = do
  drawTile' addr (x, x+8) (y, y+8)
  drawRow (n-1) (x,y) (addr + 0x00000020)

