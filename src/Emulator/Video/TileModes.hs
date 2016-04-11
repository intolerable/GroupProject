module Emulator.Video.TileModes where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Util
import Emulator.Video.VideoController

import Data.Array.MArray
import Data.Array.Storable
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
textBG bgCNTAddr xOffAddr yOffAddr = do
  bg <- recordBGControl bgCNTAddr
  bgOffset <- recordBGOffset xOffAddr yOffAddr
  let xOff = -(fromIntegral (xOffset bgOffset) :: GLdouble)
  let yOff = -(fromIntegral (yOffset bgOffset) :: GLdouble)
  let charBlock = getTileBlock $ characterBaseBlock bg
  let mapBlock = getMapBlock $ screenBaseBlock bg
  let col = colorsPalettes bg
  drawTextBG (fromIntegral (screenSize bg)) col mapBlock charBlock
  return ()

getTileBlock :: Byte -> Address
getTileBlock tileBase = 0x06000000 + (0x00004000 * (fromIntegral tileBase))

getMapBlock :: Byte -> Address
getMapBlock mapBase = 0x06000000 + (0x00000800 * (fromIntegral mapBase))

drawTextBG :: AddressSpace m => Int -> Bool -> Address -> Address -> m ()
drawTextBG 0 True mapBlock charBlock = do
  map0 <- readTileMap mapBlock
  return ()
drawTextBG 0 False mapBlock charBlock = do
  return ()
drawTextBG 1 True mapBlock charBlock = do
  map0 <- readTileMap mapBlock
  map1 <- readTileMap (mapBlock + 0x00000800)
  return ()
drawTextBG 1 False mapBlock charBlock = do
  return ()
drawTextBG 2 True mapBlock charBlock = do
  map0 <- readTileMap mapBlock
  map1 <- readTileMap (mapBlock + 0x00000800)
  return ()
drawTextBG 2 False mapBlock charBlock = do
  return ()
drawTextBG _ True mapBlock charBlock = do
  map0 <- readTileMap mapBlock
  map1 <- readTileMap (mapBlock + 0x00000800)
  map2 <- readTileMap (mapBlock + 0x00001000)
  map3 <- readTileMap (mapBlock + 0x00001800)
  return ()
drawTextBG _ False mapBlock charBlock = do
  return ()

readTileMap :: AddressSpace m => Address -> m ()
readTileMap addr = undefined

-- Draw 32x32 tiles at a time
drawTileMap :: Int -> Int -> Address -> Address -> (GLdouble, GLdouble) -> IO ()
drawTileMap 0 _ _ _ _ = return ()
drawTileMap rows columns tileMap tileSet (xOff, yOff) = undefined

drawHLine' :: Int -> Address -> Address -> (GLdouble, GLdouble) -> IO ()
drawHLine' 0 _ _ _ = return ()
drawHLine' columns tileMap tileSet (xOff, yOff) = undefined

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

-- Returns number of tiles to be drawn
affineBGSize :: Byte -> (Int, Int)
affineBGSize byt
  | byt == 0 = (16, 16)
  | byt == 1 = (32, 32)
  | byt == 2 = (64, 64)
  | otherwise = (128, 128)
