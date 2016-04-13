module Emulator.Video.TileModes where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.VideoController

import Control.Monad.IO.Class
import Data.Array.IArray
import Graphics.Rendering.OpenGL

type AddressIO m = (AddressSpace m, MonadIO m)
type Palette = Array Address Byte
type PixFormat = Bool
type TextBGOffset = (GLdouble, GLdouble)
type TileMap = Array Address Byte
type TileSet = Array Address Byte

tileModes :: AddressIO m => LCDControl -> m ()
tileModes cnt = do
  palette <- readRange (0x05000000, 0x050001FF)
  case bgMode cnt of
    0 -> mode0 palette cnt
    _ -> undefined

mode0 :: AddressIO m => Palette -> LCDControl -> m ()
mode0 palette _ = do
  textBG 0x04000008 0x04000010 0x04000012 palette
  textBG 0x0400000A 0x04000014 0x04000016 palette
  textBG 0x0400000C 0x04000018 0x0400001A palette
  textBG 0x0400000E 0x0400001C 0x0400001E palette

mode1 :: AddressIO m => Palette -> LCDControl -> m ()
mode1 palette _ = do
  textBG 0x04000008 0x04000010 0x04000012 palette
  textBG 0x0400000A 0x04000014 0x04000016 palette
  affineBG

mode2 :: AddressIO m => LCDControl -> m ()
mode2 _ = do
  affineBG
  affineBG

-- Text Mode
textBG :: AddressIO m => Address -> Address -> Address -> Palette -> m ()
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
drawTextBG :: AddressIO m => Int -> PixFormat -> Address -> Address -> TextBGOffset -> Palette -> m ()
drawTextBG 0 _pixFormat mapBlock charBlock (_xOff, _yOff) _palette = do
  _map0 <- readTileMap mapBlock
  _tileSet <- readCharBlocks charBlock False
  return ()
drawTextBG 1 _pixFormat mapBlock charBlock (_xOff, _yOff) _palette = do
  _map0 <- readTileMap mapBlock
  _map1 <- readTileMap (mapBlock + 0x00000800)
  _tileSet <- readCharBlocks charBlock False
  return ()
drawTextBG 2 _pixFormat mapBlock charBlock (_xOff, _yOff) _palette = do
  _map0 <- readTileMap mapBlock
  _map1 <- readTileMap (mapBlock + 0x00000800)
  _tileSet <- readCharBlocks charBlock False
  return ()
drawTextBG _ _pixFormat mapBlock charBlock (_xOff, _yOff) _palette = do
  _map0 <- readTileMap mapBlock
  _map1 <- readTileMap (mapBlock + 0x00000800)
  _map2 <- readTileMap (mapBlock + 0x00001000)
  _map3 <- readTileMap (mapBlock + 0x00001800)
  _tileSet <- readCharBlocks charBlock False
  return ()

  -- | byt == 0 = (32, 32)
  -- | byt == 1 = (64, 32)
  -- | byt == 2 = (32, 64)
  -- | otherwise = (64, 64)

readTileMap :: AddressIO m => Address -> m (TileMap)
readTileMap addr = do
  memBlock <- readRange (addr, addr + 0x000007FF)
  --mapMem <- liftIO $ thaw memBlock
  return memBlock

readCharBlocks :: AddressIO m => Address -> PixFormat -> m (TileSet)
readCharBlocks addr False = do
  memBlock <- readRange (addr, addr + 0x00007FFF)
  --charMem <- liftIO $ thaw memBlock
  return memBlock
readCharBlocks addr True = do
  memBlock <- readRange (addr, addr + 0x0000FFFF)
  --charMem <- liftIO $ thaw memBlock
  return memBlock

-- Draw 32x32 tiles at a time
drawTileMap :: AddressIO m => Int -> Int -> PixFormat -> TileMap -> TileSet -> TextBGOffset -> Palette -> Address -> m ()
drawTileMap 0 _ _ _ _ _ _ _ = return ()
drawTileMap rows columns colFormat tileMap tileSet bgOffset palette baseAddr = do
  let tileMapRow = ixmap (baseAddr, baseAddr + 0x0000003F) (id) tileMap :: TileMap
  drawHLine columns colFormat tileMapRow tileSet bgOffset palette
  drawTileMap (rows-1) columns colFormat tileMap tileSet bgOffset palette (baseAddr + 0x00000040)
  return ()

-- Draw 32 tile row. call drawTile to draw each tile
drawHLine :: AddressIO m => Int -> PixFormat -> TileMap -> TileSet -> TextBGOffset -> Palette -> m ()
drawHLine 0 _ _ _ _ _ = return ()
drawHLine _columns _colFormat _tileMapRow _tileSet (_xOff, _yOff) _palette = undefined

convIntToAddr :: Int -> PixFormat -> Address
convIntToAddr n False = (0x00000020 + 0x00000020 * fromIntegral n)
convIntToAddr n True = (0x00000040 + 0x00000040 * fromIntegral n)

-- drawVLines :: String -> Int -> Int -> TextBGOffset -> Address -> IO ()
-- drawVLines _ 0 _ _ _ = return ()
-- drawVLines fname n columns (x, y) addr = do
--   drawHLine fname columns (x, y) addr
--   drawVLines fname (n-1) columns (x, y+8) addr

affineBG :: AddressIO m => m ()
affineBG = undefined

-- Returns number of tiles to be drawn
affineBGSize :: Byte -> (Int, Int)
affineBGSize byt
  | byt == 0 = (16, 16)
  | byt == 1 = (32, 32)
  | byt == 2 = (64, 64)
  | otherwise = (128, 128)
