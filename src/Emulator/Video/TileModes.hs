module Emulator.Video.TileModes where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.VideoController

import Control.Monad.IO.Class
import Data.Array.IArray
import Data.Bits
import Graphics.Rendering.OpenGL
import Utilities.Parser.TemplateHaskell

type AddressIO m = (AddressSpace m, MonadIO m)
type Palette = Array Address Byte
type PixFormat = Bool
type TextBGOffset = (GLdouble, GLdouble)
type TileMap = Array Address Byte
type TileSet = Array Address Byte
type MapBaseAddress = Address
type SetBaseAddress = Address

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
  let tileSetAddr = baseTileSetAddr $ characterBaseBlock bg
  let tileMapAddr = baseTileMapAddr $ screenBaseBlock bg
  let paletteFormat = colorsPalettes bg
  drawTextBG (fromIntegral (screenSize bg)) paletteFormat tileMapAddr tileSetAddr (xOff, yOff) palette
  return ()

-- Gets the base memory addres for the tile
baseTileSetAddr :: Byte -> SetBaseAddress
baseTileSetAddr tileBase = 0x06000000 + (0x00004000 * (fromIntegral tileBase))

baseTileMapAddr :: Byte -> MapBaseAddress
baseTileMapAddr mapBase = 0x06000000 + (0x00000800 * (fromIntegral mapBase))

-- if False then Colour is 4bpp aka S-tiles
drawTextBG :: AddressIO m => Int -> PixFormat -> MapBaseAddress -> SetBaseAddress -> TextBGOffset -> Palette -> m ()
drawTextBG 0 pixFormat tileMapAddr tileSetAddr offSet palette = do
  map0 <- readTileMap tileMapAddr
  tileSet <- readCharBlocks tileSetAddr False
  drawTileMap 32 pixFormat map0 tileSet offSet palette tileMapAddr tileSetAddr
  return ()
drawTextBG 1 _pixFormat tileMapAddr tileSetAddr (_xOff, _yOff) _palette = do
  _map0 <- readTileMap tileMapAddr
  _map1 <- readTileMap (tileMapAddr + 0x00000800)
  _tileSet <- readCharBlocks tileSetAddr False
  return ()
drawTextBG 2 _pixFormat tileMapAddr tileSetAddr (_xOff, _yOff) _palette = do
  _map0 <- readTileMap tileMapAddr
  _map1 <- readTileMap (tileMapAddr + 0x00000800)
  _tileSet <- readCharBlocks tileSetAddr False
  return ()
drawTextBG _ _pixFormat tileMapAddr tileSetAddr (_xOff, _yOff) _palette = do
  _map0 <- readTileMap tileMapAddr
  _map1 <- readTileMap (tileMapAddr + 0x00000800)
  _map2 <- readTileMap (tileMapAddr + 0x00001000)
  _map3 <- readTileMap (tileMapAddr + 0x00001800)
  _tileSet <- readCharBlocks tileSetAddr False
  return ()

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
drawTileMap :: AddressIO m => Int -> PixFormat -> TileMap -> TileSet -> TextBGOffset -> Palette -> MapBaseAddress -> SetBaseAddress -> m ()
drawTileMap 0 _ _ _ _ _ _ _ = return ()
drawTileMap rows colFormat tileMap tileSet bgOffset palette baseAddr setBaseAddr = do
  let tileMapRow = ixmap (baseAddr, baseAddr + 0x0000003F) (id) tileMap :: TileMap
  drawHLine 0x00000000 colFormat tileMapRow tileSet bgOffset palette setBaseAddr
  drawTileMap (rows-1) colFormat tileMap tileSet bgOffset palette (baseAddr + 0x00000040) setBaseAddr
  return ()

drawHLine :: AddressIO m => Address -> PixFormat -> TileMap -> TileSet -> TextBGOffset -> Palette -> SetBaseAddress -> m ()
drawHLine 0x00000040 _ _ _ _ _ _ = return ()
drawHLine columns colFormat tileMapRow tileSet (xOff, yOff) palette setBaseAddr = do
  let _ = parseScreenEntry (tileMapRow!columns) (tileMapRow!(columns + 0x00000001)) colFormat setBaseAddr

  drawHLine (columns + 0x00000001) colFormat tileMapRow tileSet (xOff + 8, yOff) palette setBaseAddr
  return ()

parseScreenEntry :: Byte -> Byte -> PixFormat -> SetBaseAddress -> (Address, Bool, Bool, Int)
parseScreenEntry a b pixFormat setBaseAddr = (tileIdx, hFlip, vFlip, palBank)
  where
    hword = ((fromIntegral a :: HalfWord) `shiftL` 8) .|. ((fromIntegral b :: HalfWord) .&. 0xFF) :: HalfWord
    tileIdx = setBaseAddr + convIntToAddr (fromIntegral $ $(bitmask 9 0) hword :: Int) pixFormat
    hFlip = (testBit hword 10)
    vFlip = (testBit hword 11)
    palBank = (fromIntegral $ $(bitmask 15 12) hword :: Int)

convIntToAddr :: Int -> PixFormat -> Address
convIntToAddr n False = (0x00000020 + 0x00000020 * fromIntegral n)
convIntToAddr n True = (0x00000040 + 0x00000040 * fromIntegral n)

affineBG :: AddressIO m => m ()
affineBG = undefined

-- Returns number of tiles to be drawn
affineBGSize :: Byte -> (Int, Int)
affineBGSize byt
  | byt == 0 = (16, 16)
  | byt == 1 = (32, 32)
  | byt == 2 = (64, 64)
  | otherwise = (128, 128)
