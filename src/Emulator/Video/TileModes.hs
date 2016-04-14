module Emulator.Video.TileModes where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.VideoController

import Control.Monad.IO.Class
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.Storable
import Data.Bits
import Graphics.Rendering.OpenGL
import Utilities.Parser.TemplateHaskell

type AddressIO m = (AddressSpace m, MonadIO m)
type TileMapBaseAddress = Address
type Palette = Array Address Byte
type PixFormat = Bool
type ScreenEntry = (Address, Bool, Bool, Address)
type TileSetBaseAddress = Address
type TextBGOffset = (GLdouble, GLdouble)
type Tile = Array Address Byte
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
drawTextBG :: AddressIO m => Int -> PixFormat -> TileMapBaseAddress -> TileSetBaseAddress -> TextBGOffset -> Palette -> m ()
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
drawTileMap :: AddressIO m => Int -> PixFormat -> TileMap -> TileSet -> TextBGOffset -> Palette -> TileMapBaseAddress -> TileSetBaseAddress -> m ()
drawTileMap 0 _ _ _ _ _ _ _ = return ()
drawTileMap rows pixFormat tileMap tileSet bgOffset palette baseAddr setBaseAddr = do
  let tileMapRow = ixmap (baseAddr, baseAddr + 0x0000003F) (id) tileMap :: TileMap
  drawHLine 0x00000000 pixFormat tileMapRow tileSet bgOffset palette setBaseAddr
  drawTileMap (rows-1) pixFormat tileMap tileSet bgOffset palette (baseAddr + 0x00000040) setBaseAddr
  return ()

drawHLine :: AddressIO m => Address -> PixFormat -> TileMap -> TileSet -> TextBGOffset -> Palette -> TileSetBaseAddress -> m ()
drawHLine 0x00000040 _ _ _ _ _ _ = return ()
drawHLine mapIndex pixFormat tileMapRow tileSet (xOff, yOff) palette setBaseAddr = do
  let _tile = getTile pixFormat tileIdx tileSet
  -- pixelData
  drawHLine (mapIndex + 0x00000002) pixFormat tileMapRow tileSet (xOff + 8, yOff) palette setBaseAddr
  return ()
  where
    upperByte = (tileMapRow!(mapIndex + 0x00000001))
    lowerByte = (tileMapRow!mapIndex)
    (tileIdx, _, _, _) = parseScreenEntry upperByte lowerByte pixFormat setBaseAddr

pixelData :: AddressIO m => PixFormat -> Palette -> Tile -> Address -> m (StorableArray Address HalfWord)
-- 256/1 palette format
pixelData True _palette _tile _ = undefined
-- 16/16 palette format
pixelData _ palette tile palBank = do
  let bank = ixmap (palBankAddr, (palBankAddr + 0x0000001F)) (id) palette :: Palette
  let tilePixelDataList = palette16 bank tile palBankAddr (fst tileBounds) 0
  tilePixelData <- liftIO $ newListArray tileBounds tilePixelDataList
  return tilePixelData
  where
    tileBounds = bounds tile
    palBankAddr = 0x05000000 + palBank

palette16 :: Palette -> Tile -> Address -> Address -> Int -> [HalfWord]
palette16 _ _ _ _ 32 = []
palette16 bank tile palBankBaseAddr tileAddr n = col1:col2:palette16 bank tile palBankBaseAddr tileAddr (n+1)
  where
    byt = tile!(tileAddr + (0x00000001 * fromIntegral n))
    nib1 = fromIntegral $ $(bitmask 3 0) byt :: Address
    nib2 = fromIntegral $ $(bitmask 7 4) byt :: Address
    col1Byt1 = bank!(nib1 + palBankBaseAddr)
    col1Byt2 = bank!(nib1 + palBankBaseAddr + 0x00000001)
    col1 = ((fromIntegral col1Byt2 :: HalfWord) `shiftL` 8) .|. ((fromIntegral col1Byt1 :: HalfWord) .&. 0xFF) :: HalfWord
    col2Byt1 = bank!(nib2 + palBankBaseAddr)
    col2Byt2 = bank!(nib2 + palBankBaseAddr + 0x00000001)
    col2 = ((fromIntegral col2Byt2 :: HalfWord) `shiftL` 8) .|. ((fromIntegral col2Byt1 :: HalfWord) .&. 0xFF) :: HalfWord

-- a is the upper byte, b is the lower
parseScreenEntry :: Byte -> Byte -> PixFormat -> TileSetBaseAddress -> ScreenEntry
parseScreenEntry a b pixFormat setBaseAddr = (tileIdx, hFlip, vFlip, palBank)
  where
    hword = ((fromIntegral a :: HalfWord) `shiftL` 8) .|. ((fromIntegral b :: HalfWord) .&. 0xFF) :: HalfWord
    tileIdx = setBaseAddr + convIntToAddr (fromIntegral $ $(bitmask 9 0) hword :: Int) pixFormat
    hFlip = (testBit hword 10)
    vFlip = (testBit hword 11)
    palBank = convIntToAddr (fromIntegral $ $(bitmask 15 12) hword :: Int) False

-- If pixel format is 8bpp then TileSet is read in chunks of 40h
-- If not then TileSet is read in chunks of 20h
getTile :: PixFormat -> Address -> TileSet -> Tile
getTile True tileIdx tileSet = (ixmap (tileIdx, tileIdx + 0x0000003F) (id) tileSet :: Tile)
getTile _ tileIdx tileSet = (ixmap (tileIdx, tileIdx + 0x0000001F) (id) tileSet :: Tile)

-- If pixel format is 8bpp then the tileIndex read from the map is in steps of 40h
-- If pixel format is 4bpp then the tileIndex read from the map is in steps of 20h
convIntToAddr :: Int -> PixFormat -> Address
convIntToAddr 0 _ = 0x00000000
convIntToAddr n True = (0x00000040 * fromIntegral n)
convIntToAddr n _ = (0x00000020 * fromIntegral n)

affineBG :: AddressIO m => m ()
affineBG = undefined

-- Returns number of tiles to be drawn
affineBGSize :: Byte -> (Int, Int)
affineBGSize byt
  | byt == 0 = (16, 16)
  | byt == 1 = (32, 32)
  | byt == 2 = (64, 64)
  | otherwise = (128, 128)
