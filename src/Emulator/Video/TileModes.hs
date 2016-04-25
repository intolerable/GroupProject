module Emulator.Video.TileModes where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Palette
import Emulator.Video.Util
import Emulator.Video.VideoController
import Utilities.Parser.TemplateHaskell

import Control.Monad.IO.Class
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.Storable
import Data.Bits
import Graphics.Rendering.OpenGL

type ScreenEntry = (Address, Bool, Bool, Address)
type TileMapBaseAddress = Address
type TileMap = Array Address Byte

tileModes :: AddressIO m => LCDControl -> m ()
tileModes cnt = do
  palette <- readRange (0x05000000, 0x050001FF)
  case bgMode cnt of
    0 -> mode0 palette cnt
    1 -> mode1 palette cnt
    _ -> mode2 palette cnt

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

mode2 :: AddressIO m => Palette -> LCDControl -> m ()
mode2 _ _ = do
  affineBG
  affineBG

-- Text Mode
textBG :: AddressIO m => Address -> Address -> Address -> Palette -> m ()
textBG bgCNTAddr xOffAddr yOffAddr palette = do
  bg <- recordBGControl bgCNTAddr
  bgOffset <- recordBGOffset xOffAddr yOffAddr
  let xOff = -(fromIntegral (xOffset bgOffset) :: GLdouble)
  let yOff = -(fromIntegral (yOffset bgOffset) :: GLdouble)
  let tileSetAddr = baseTileSetAddr $ characterBaseBlock bg
  let tileMapAddr = baseTileMapAddr $ screenBaseBlock bg
  let paletteFormat = colorsPalettes bg
  drawTextBG (fromIntegral (screenSize bg)) paletteFormat tileMapAddr tileSetAddr (xOff, yOff) palette
  return ()

-- Gets the base memory addres for the tile
baseTileSetAddr :: Byte -> TileSetBaseAddress
baseTileSetAddr tileBase = 0x06000000 + (0x00004000 * (fromIntegral tileBase))

baseTileMapAddr :: Byte -> TileMapBaseAddress
baseTileMapAddr mapBase = 0x06000000 + (0x00000800 * (fromIntegral mapBase))

-- if False then Colour is 4bpp aka S-tiles
drawTextBG :: AddressIO m => Int -> PixFormat -> TileMapBaseAddress -> TileSetBaseAddress -> TileOffset -> Palette -> m ()
drawTextBG 0 pixFormat tileMapAddr tileSetAddr offSet palette = do
  tileMap0 <- readTileMap tileMapAddr
  tileSet <- readCharBlocks tileSetAddr False
  drawTileMap 32 pixFormat tileMap0 tileSet offSet palette tileMapAddr tileSetAddr
  return ()
drawTextBG 1 pixFormat tileMapAddr tileSetAddr offSet@(xOff, yOff) palette = do
  tileMap0 <- readTileMap tileMapAddr
  tileMap1 <- readTileMap (tileMapAddr + 0x00000800)
  tileSet <- readCharBlocks tileSetAddr False
  drawTileMap 32 pixFormat tileMap0 tileSet offSet palette tileMapAddr tileSetAddr
  drawTileMap 32 pixFormat tileMap1 tileSet (xOff + 32, yOff) palette tileMapAddr tileSetAddr
  return ()
drawTextBG 2 pixFormat tileMapAddr tileSetAddr offSet@(xOff, yOff) palette = do
  tileMap0 <- readTileMap tileMapAddr
  tileMap1 <- readTileMap (tileMapAddr + 0x00000800)
  tileSet <- readCharBlocks tileSetAddr False
  drawTileMap 32 pixFormat tileMap0 tileSet offSet palette tileMapAddr tileSetAddr
  drawTileMap 32 pixFormat tileMap1 tileSet (xOff, yOff + 32) palette tileMapAddr tileSetAddr
  return ()
drawTextBG _ pixFormat tileMapAddr tileSetAddr offSet@(xOff, yOff) palette = do
  tileMap0 <- readTileMap tileMapAddr
  tileMap1 <- readTileMap (tileMapAddr + 0x00000800)
  tileMap2 <- readTileMap (tileMapAddr + 0x00001000)
  tileMap3 <- readTileMap (tileMapAddr + 0x00001800)
  tileSet <- readCharBlocks tileSetAddr False
  drawTileMap 32 pixFormat tileMap0 tileSet offSet palette tileMapAddr tileSetAddr
  drawTileMap 32 pixFormat tileMap1 tileSet (xOff + 32, yOff) palette tileMapAddr tileSetAddr
  drawTileMap 32 pixFormat tileMap2 tileSet (xOff, yOff + 32) palette tileMapAddr tileSetAddr
  drawTileMap 32 pixFormat tileMap3 tileSet (xOff + 32, yOff + 32) palette tileMapAddr tileSetAddr
  return ()

readTileMap :: AddressSpace m => Address -> m (TileMap)
readTileMap addr = readRange (addr, addr + 0x000007FF)

readCharBlocks :: AddressSpace m => Address -> PixFormat -> m (TileSet)
readCharBlocks addr False = readRange (addr, addr + 0x00007FFF)
readCharBlocks addr True = readRange (addr, addr + 0x0000FFFF)

-- Draw 32x32 tiles at a time
drawTileMap :: AddressIO m => Int -> PixFormat -> TileMap -> TileSet -> TileOffset -> Palette -> TileMapBaseAddress -> TileSetBaseAddress -> m ()
drawTileMap 0 _ _ _ _ _ _ _ = return ()
drawTileMap rows pixFormat tileMap tileSet bgOffset@(xOff, yOff) palette baseAddr setBaseAddr = do
  let tileMapRow = ixmap (baseAddr, baseAddr + 0x0000003F) (id) tileMap :: TileMap
  drawHLine 32 baseAddr pixFormat tileMapRow tileSet bgOffset palette setBaseAddr
  drawTileMap (rows-1) pixFormat tileMap tileSet (xOff, yOff + 8) palette (baseAddr + 0x00000040) setBaseAddr
  return ()

-- Need to recurse using int instead
drawHLine :: AddressIO m => Int -> Address -> PixFormat -> TileMap -> TileSet -> TileOffset -> Palette -> TileSetBaseAddress -> m ()
drawHLine 0 _ _ _ _ _ _ _ = return ()
drawHLine column mapIndex pixFormat tileMapRow tileSet (xOff, yOff) palette setBaseAddr = do
  let tile = getTile pixFormat tileIdx tileSet
  pixData <- pixelData pixFormat palette tile palBank
  liftIO $ drawTile pixData (xOff, xOff + 8) (yOff, yOff + 8)
  drawHLine (column-1) (mapIndex + 0x00000002) pixFormat tileMapRow tileSet (xOff + 8, yOff) palette setBaseAddr
  return ()
  where
    upperByte = (tileMapRow!(mapIndex + 0x00000001))
    lowerByte = (tileMapRow!mapIndex)
    (tileIdx, _hFlip, _vFlip, palBank) = parseScreenEntry upperByte lowerByte pixFormat setBaseAddr
-- NEED TO SORT HFLIP AND VFLIP WHEN GRAPHICS RUN

-- a is the upper byte, b is the lower
parseScreenEntry :: Byte -> Byte -> PixFormat -> TileSetBaseAddress -> ScreenEntry
parseScreenEntry a b pixFormat setBaseAddr = (tileIdx, hFlip, vFlip, palBank)
  where
    hword = bytesToHalfWord b a
    tileIdx = setBaseAddr + convIntToAddr (fromIntegral $ $(bitmask 9 0) hword :: Int) pixFormat
    hFlip = (testBit hword 10)
    vFlip = (testBit hword 11)
    palBank = convIntToAddr (fromIntegral $ $(bitmask 15 12) hword :: Int) False

affineBG :: AddressIO m => m ()
affineBG = undefined

-- Returns number of tiles to be drawn
affineBGSize :: Byte -> (Int, Int)
affineBGSize byt
  | byt == 0 = (16, 16)
  | byt == 1 = (32, 32)
  | byt == 2 = (64, 64)
  | otherwise = (128, 128)
