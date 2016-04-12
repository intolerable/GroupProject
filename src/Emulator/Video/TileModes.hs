module Emulator.Video.TileModes where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Util
import Emulator.Video.VideoController

import Control.Monad.IO.Class
import Data.Array.MArray
import Data.Array.Storable
import Graphics.Rendering.OpenGL

tileModes :: (AddressSpace m, MonadIO m) => LCDControl -> m ()
tileModes cnt = do
  palette <- readRange (0x05000000, 0x050001FF)
  case bgMode cnt of
    0 -> mode0 palette cnt
    _ -> undefined

mode0 :: (AddressSpace m, MonadIO m) => Palette -> LCDControl -> m ()
mode0 palette _ = do
  textBG 0x04000008 0x04000010 0x04000012 palette
  textBG 0x0400000A 0x04000014 0x04000016 palette
  textBG 0x0400000C 0x04000018 0x0400001A palette
  textBG 0x0400000E 0x0400001C 0x0400001E palette

mode1 :: (AddressSpace m, MonadIO m) => Palette -> LCDControl -> m ()
mode1 palette _ = do
  textBG 0x04000008 0x04000010 0x04000012 palette
  textBG 0x0400000A 0x04000014 0x04000016 palette
  affineBG

mode2 :: (AddressSpace m, MonadIO m) => LCDControl -> m ()
mode2 _ = do
  affineBG
  affineBG

-- Text Mode
textBG :: (AddressSpace m, MonadIO m) => Address -> Address -> Address -> Palette -> m ()
textBG bgCNTAddr xOffAddr yOffAddr palette = do
  bg <- recordBGControl bgCNTAddr
  bgOffset <- recordBGOffset xOffAddr yOffAddr
  let xOff = -(fromIntegral (xOffset bgOffset) :: GLdouble)
  let yOff = -(fromIntegral (yOffset bgOffset) :: GLdouble)
  let charBlock = getBaseCharBlock $ characterBaseBlock bg
  let mapBlock = getBaseMapBlock $ screenBaseBlock bg
  let paletteFormat = colorsPalettes bg
  drawTextBG (fromIntegral (screenSize bg)) paletteFormat mapBlock charBlock (xOff, yOff) palette
  return ()

-- Gets the base memory addres for the tile
getBaseCharBlock :: Byte -> Address
getBaseCharBlock tileBase = 0x06000000 + (0x00004000 * (fromIntegral tileBase))

getBaseMapBlock :: Byte -> Address
getBaseMapBlock mapBase = 0x06000000 + (0x00000800 * (fromIntegral mapBase))

-- if False then Colour is 4bpp aka S-tiles
drawTextBG :: (AddressSpace m, MonadIO m) => Int -> Bool -> Address -> Address -> TextBGOffset -> Palette -> m ()
drawTextBG 0 False mapBlock charBlock (xOff, yOff) palette = do
  map0 <- readTileMap mapBlock
  tileset <- readCharBlocks charBlock False
  return ()
drawTextBG 0 True mapBlock charBlock (xOff, yOff) palette = do
  return ()
drawTextBG 1 False mapBlock charBlock (xOff, yOff) palette = do
  map0 <- readTileMap mapBlock
  map1 <- readTileMap (mapBlock + 0x00000800)
  tileset <- readCharBlocks charBlock False
  return ()
drawTextBG 1 True mapBlock charBlock (xOff, yOff) palette = do
  return ()
drawTextBG 2 False mapBlock charBlock (xOff, yOff) palette = do
  map0 <- readTileMap mapBlock
  map1 <- readTileMap (mapBlock + 0x00000800)
  tileset <- readCharBlocks charBlock False
  return ()
drawTextBG 2 True mapBlock charBlock (xOff, yOff) palette = do
  return ()
drawTextBG _ False mapBlock charBlock (xOff, yOff) palette = do
  map0 <- readTileMap mapBlock
  map1 <- readTileMap (mapBlock + 0x00000800)
  map2 <- readTileMap (mapBlock + 0x00001000)
  map3 <- readTileMap (mapBlock + 0x00001800)
  tileset <- readCharBlocks charBlock False
  return ()
drawTextBG _ True mapBlock charBlock (xOff, yOff) palette = do
  return ()

  -- | byt == 0 = (32, 32)
  -- | byt == 1 = (64, 32)
  -- | byt == 2 = (32, 64)
  -- | otherwise = (64, 64)

readTileMap :: (AddressSpace m, MonadIO m) => Address -> m (TileMap)
readTileMap addr = do
  memBlock <- readRange (addr, addr + 0x000007FF)
  mapMem <- liftIO $ thaw memBlock
  return mapMem

readCharBlocks :: (AddressSpace m, MonadIO m) => Address -> Bool -> m (TileSet)
readCharBlocks addr False = do
  memBlock <- readRange (addr, addr + 0x00007FFF)
  charMem <- liftIO $ thaw memBlock
  return charMem
readCharBlocks addr True = do
  memBlock <- readRange (addr, addr + 0x0000FFFF)
  charMem <- liftIO $ thaw memBlock
  return charMem

-- Draw 32x32 tiles at a time
drawTileMap :: Int -> Int -> TileMap -> TileSet -> TextBGOffset -> IO ()
drawTileMap 0 _ _ _ _ = return ()
drawTileMap rows columns tileMap tileSet (xOff, yOff) = undefined

-- Draw 32 tile row. call drawTile to draw each tile
drawHLine :: Int -> TileMap -> TileSet -> TextBGOffset -> IO ()
drawHLine 0 _ _ _ = return ()
drawHLine columns tileMapRow tileSet (xOff, yOff) = undefined

--drawTile' ::

-- drawVLines :: String -> Int -> Int -> TextBGOffset -> Address -> IO ()
-- drawVLines _ 0 _ _ _ = return ()
-- drawVLines fname n columns (x, y) addr = do
--   drawHLine fname columns (x, y) addr
--   drawVLines fname (n-1) columns (x, y+8) addr

affineBG :: (AddressSpace m, MonadIO m) => m ()
affineBG = undefined

-- Returns number of tiles to be drawn
affineBGSize :: Byte -> (Int, Int)
affineBGSize byt
  | byt == 0 = (16, 16)
  | byt == 1 = (32, 32)
  | byt == 2 = (64, 64)
  | otherwise = (128, 128)
