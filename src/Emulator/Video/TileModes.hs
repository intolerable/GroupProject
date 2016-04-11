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
mode0 _ = do
  textBG 0x04000008 0x04000010 0x04000012
  return ()

-- Text Mode
textBG :: AddressSpace m => Address -> Address -> Address -> m ()
textBG bgCNT xOff yOff = do
  bg <- recordBGControl bgCNT
  bgOffset <- recordBGOffset xOff yOff
  let bgSize = textBGSize $ screenSize bg
  let tileData = getTileBlock $ characterBaseBlock bg
  let mapBlock = getMapBlock $ screenBaseBlock bg
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
getTileBlock tileBase = 0x06000000 + (0x00004000 * (fromIntegral tileBase))

getMapBlock :: Byte -> Address
getMapBlock mapBase = 0x06000000 + (0x00000800 * (fromIntegral mapBase))

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
