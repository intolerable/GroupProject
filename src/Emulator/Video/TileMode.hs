module Emulator.Video.TileMode where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Palette
import Emulator.Video.Util
import Emulator.Video.VideoController
import Utilities.Parser.TemplateHaskell

import Control.Monad.IO.Class
import Data.Array.IArray
import Data.Bits
import Graphics.Rendering.OpenGL

type ScreenEntry = (Address, Bool, Bool, Address)
type TileMapBaseAddress = Address
type TileMap = Array Address Byte
type AffineRefPoints = (GLdouble, GLdouble)

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
  affineBG 0x0400000C 0x04000028 0x04000020 palette

mode2 :: AddressIO m => Palette -> LCDControl -> m ()
mode2 palette _ = do
  affineBG 0x0400000C 0x04000028 0x04000020 palette
  affineBG 0x0400000E 0x04000038 0x04000030 palette

-- Text Mode
textBG :: AddressIO m => Address -> Address -> Address -> Palette -> m ()
textBG bgCNTAddr xOffAddr yOffAddr palette = do
  bg <- recordBGControl bgCNTAddr
  xHWord <- readAddressHalfWord xOffAddr
  yHWord <- readAddressHalfWord yOffAddr
  let xOff = negate (fromIntegral $ $(bitmask 8 0) xHWord) :: GLdouble
  let yOff = negate (fromIntegral $ $(bitmask 8 0) yHWord) :: GLdouble
  drawTextBG (screenSize bg) (colorsPalettes bg) (screenBaseBlock bg) (characterBaseBlock bg) (xOff, yOff) palette
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
  tileSet <- readCharBlocks tileSetAddr pixFormat
  drawTileMap (32, 32) pixFormat tileMap0 tileSet offSet palette tileMapAddr tileSetAddr
  return ()
drawTextBG 1 pixFormat tileMapAddr tileSetAddr offSet@(xOff, yOff) palette = do
  tileMap0 <- readTileMap tileMapAddr
  tileMap1 <- readTileMap (tileMapAddr + 0x00000800)
  tileSet <- readCharBlocks tileSetAddr pixFormat
  drawTileMap (32, 32) pixFormat tileMap0 tileSet offSet palette tileMapAddr tileSetAddr
  drawTileMap (32, 32) pixFormat tileMap1 tileSet (xOff + 32, yOff) palette tileMapAddr tileSetAddr
  return ()
drawTextBG 2 pixFormat tileMapAddr tileSetAddr offSet@(xOff, yOff) palette = do
  tileMap0 <- readTileMap tileMapAddr
  tileMap1 <- readTileMap (tileMapAddr + 0x00000800)
  tileSet <- readCharBlocks tileSetAddr pixFormat
  drawTileMap (32, 32) pixFormat tileMap0 tileSet offSet palette tileMapAddr tileSetAddr
  drawTileMap (32, 32) pixFormat tileMap1 tileSet (xOff, yOff + 32) palette tileMapAddr tileSetAddr
  return ()
drawTextBG _ pixFormat tileMapAddr tileSetAddr offSet@(xOff, yOff) palette = do
  tileMap0 <- readTileMap tileMapAddr
  tileMap1 <- readTileMap (tileMapAddr + 0x00000800)
  tileMap2 <- readTileMap (tileMapAddr + 0x00001000)
  tileMap3 <- readTileMap (tileMapAddr + 0x00001800)
  tileSet <- readCharBlocks tileSetAddr pixFormat
  drawTileMap (32, 32) pixFormat tileMap0 tileSet offSet palette tileMapAddr tileSetAddr
  drawTileMap (32, 32) pixFormat tileMap1 tileSet (xOff + 32, yOff) palette tileMapAddr tileSetAddr
  drawTileMap (32, 32) pixFormat tileMap2 tileSet (xOff, yOff + 32) palette tileMapAddr tileSetAddr
  drawTileMap (32, 32) pixFormat tileMap3 tileSet (xOff + 32, yOff + 32) palette tileMapAddr tileSetAddr
  return ()

readTileMap :: AddressSpace m => Address -> m (TileMap)
readTileMap addr = readRange (addr, addr + 0x000007FF)

readCharBlocks :: AddressSpace m => Address -> PixFormat -> m TileSet
readCharBlocks addr False = readRange (addr, addr + 0x00007FFF)
readCharBlocks addr True = readRange (addr, addr + 0x0000FFFF)

-- Draw 32x32 tiles at a time
drawTileMap :: AddressIO m => (Int, Int) -> PixFormat -> TileMap -> TileSet -> TileOffset -> Palette -> TileMapBaseAddress -> TileSetBaseAddress -> m ()
drawTileMap (0, _) _ _ _ _ _ _ _ = return ()
drawTileMap (rows, cols) pixFormat tileMap tileSet bgOffset@(xOff, yOff) palette baseAddr setBaseAddr = do
  drawHLine cols baseAddr pixFormat tileMapRow tileSet bgOffset palette setBaseAddr
  drawTileMap (rows-1, cols) pixFormat tileMap tileSet (xOff, yOff + 8) palette (baseAddr + 0x00000040) setBaseAddr
  return ()
  where
    tileMapRow = ixmap (baseAddr, baseAddr + 0x0000003F) (id) tileMap :: TileMap

-- Need to recurse using int instead
drawHLine :: AddressIO m => Int -> Address -> PixFormat -> TileMap -> TileSet -> TileOffset -> Palette -> TileSetBaseAddress -> m ()
drawHLine 0 _ _ _ _ _ _ _ = return ()
drawHLine column mapIndex pixFormat tileMapRow tileSet (xOff, yOff) palette setBaseAddr = do
  pixData <- pixelData pixFormat palette tile palBank
  liftIO $ drawTile pixData tileCoords
  drawHLine (column-1) (mapIndex + 0x00000002) pixFormat tileMapRow tileSet (xOff + 8, yOff) palette setBaseAddr
  return ()
  where
    tile = getTile pixFormat tileIdx tileSet
    upperByte = (tileMapRow!(mapIndex + 0x00000001))
    lowerByte = (tileMapRow!mapIndex)
    (tileIdx, _hFlip, _vFlip, palBank) = parseScreenEntry upperByte lowerByte pixFormat setBaseAddr
    tileCoords = ((xOff, yOff), (xOff+8, yOff), (xOff, yOff+8), (xOff+8, yOff+8))
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

affineBG :: AddressIO m => Address -> Address -> Address -> Palette -> m ()
affineBG bgCNTAddr refBaseAddr paramBaseAddr _pal = do
  _bg <- recordBGControl bgCNTAddr
  xWord <- readAddressWord refBaseAddr
  yWord <- (readAddressWord (refBaseAddr + 0x00000004))
  paramMem <- readRange (paramBaseAddr, paramBaseAddr + 0x00000007)
  let _refPoint = (referencePoint xWord, referencePoint yWord)
  let _params = affineParameters paramBaseAddr (paramBaseAddr + 0x00000002) (paramBaseAddr + 0x00000004) (paramBaseAddr + 0x00000006) paramMem
  return ()

drawAffineBG :: AddressIO m => Int -> PixFormat -> TileMapBaseAddress -> TileSetBaseAddress -> AffineParameters -> AffineRefPoints -> Palette -> m ()
drawAffineBG 0 pixFormat tileMapAddr tileSetAddr _params _refPoints _pal = do
  _tileMap <- readTileMap tileMapAddr
  _tileSet <- readCharBlocks tileSetAddr pixFormat
  return ()
drawAffineBG 1 _pixFormat _tileMapAddr _tileSetAddr _params _refPoints _pal = undefined
drawAffineBG 2 _pixFormat _tileMapAddr _tileSetAddr _params _refPoints _pal = undefined
drawAffineBG _ _pixFormat _tileMapAddr _tileSetAddr _params _refPoints _pal = undefined

referencePoint :: MWord -> GLdouble
referencePoint word
  | sign = negate val
  | otherwise = val
  where
    val = intPor + (frac / 256)
    frac = fromIntegral $ $(bitmask 7 0) word :: GLdouble
    intPor = fromIntegral $ $(bitmask 26 8) word :: GLdouble
    sign = testBit word 27

-- Returns number of tiles to be drawn
affineBGSize :: Byte -> (Int, Int)
affineBGSize byt
  | byt == 0 = (16, 16)
  | byt == 1 = (32, 32)
  | byt == 2 = (64, 64)
  | otherwise = (128, 128)

data BGControl =       -- R/W. BGs 0-3
  BGControl { bgPriority :: Int          -- 0 = Highest
            , characterBaseBlock :: TileSetBaseAddress  -- =BG Tile Data. Indicates the start of tile counting
            , mosaic :: Bool
            , colorsPalettes :: Bool      -- (0=16/16, 1=256/1)
            , screenBaseBlock :: TileMapBaseAddress
            , displayAreaFlow :: Bool     -- BG 2 & BG 3 only
            , screenSize :: Int }
  deriving (Show, Read, Eq)

-- Reads a mem address that points to bgcnt register
recordBGControl :: AddressSpace m => Address -> m BGControl
recordBGControl addr = do
  hword <- readAddressHalfWord addr
  let bgCNT = BGControl (fromIntegral $ $(bitmask 1 0) hword)
                        (baseTileSetAddr (fromIntegral $ $(bitmask 3 2) hword))
                        (testBit hword 6)
                        (testBit hword 7)
                        (baseTileMapAddr (fromIntegral $ $(bitmask 12 8) hword))
                        (testBit hword 13)
                        (fromIntegral $ $(bitmask 15 14) hword)
  return bgCNT
