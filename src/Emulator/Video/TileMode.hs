module Emulator.Video.TileMode where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Palette
import Emulator.Video.Util
import Emulator.Video.VideoController
import Utilities.Parser.TemplateHaskell

import Data.Array.IArray
import Data.Bits
import Graphics.Rendering.OpenGL

data ScreenObj =
  NormalBG [Tile'] Priority |
  AffineBG [Tile'] Priority |
  NormalSprite [Tile'] Priority |
  AffineSprite [Tile'] Priority


data Tile' = Tile' [HalfWord] QuadCoords

type Priority = Int
type ScreenEntry = (Address, Bool, Bool, Address)
type TileMap = Array Address Byte

tileModes :: AddressIO m => LCDControl -> m ()
tileModes cnt = do
  palette <- readRange (0x05000000, 0x050001FF)
  case bgMode cnt of
    0 -> mode0 palette cnt
    1 -> mode1 palette cnt
    _ -> mode2 palette cnt

mode0 :: AddressSpace m => Palette -> LCDControl -> m ()
mode0 palette _ = do
  bg0Data <- readTextBG 0x04000008 0x04000010 0x04000012
  bg1Data <- readTextBG 0x0400000A 0x04000014 0x04000016
  bg2Data <- readTextBG 0x0400000C 0x04000018 0x0400001A
  bg3Data <- readTextBG 0x0400000E 0x0400001C 0x0400001E
  let _bg0 = textBG bg0Data palette
  let _bg1 = textBG bg1Data palette
  let _bg2 = textBG bg2Data palette
  let _bg3 = textBG bg3Data palette
  return ()


mode1 :: AddressIO m => Palette -> LCDControl -> m ()
mode1 palette _ = do
  bg0Data <- readTextBG 0x04000008 0x04000010 0x04000012
  bg1Data <- readTextBG 0x0400000A 0x04000014 0x04000016
  let _bg0 = textBG bg0Data palette
  let _bg1 = textBG bg1Data palette
  affineBG 0x0400000C 0x04000028 0x04000020 palette
  return ()

mode2 :: AddressIO m => Palette -> LCDControl -> m ()
mode2 palette _ = do
  affineBG 0x0400000C 0x04000028 0x04000020 palette
  affineBG 0x0400000E 0x04000038 0x04000030 palette

readTextBG :: AddressSpace m => Address -> Address -> Address -> m (BGControl, TileOffset, ([TileMap], TileSet))
readTextBG bgCNTAddr xOffAddr yOffAddr = do
  bg <- recordBGControl bgCNTAddr
  xHWord <- readAddressHalfWord xOffAddr
  yHWord <- readAddressHalfWord yOffAddr
  let xOff = negate (fromIntegral $ $(bitmask 8 0) xHWord) :: GLdouble
  let yOff = negate (fromIntegral $ $(bitmask 8 0) yHWord) :: GLdouble
  tileSet <- readCharBlocks (characterBaseBlock bg) (colorsPalettes bg)
  tileMapSets <- getTileMaps (screenSize bg) (screenBaseBlock bg)
  return (bg, (xOff, yOff), (tileMapSets, tileSet))

getTileMaps :: AddressSpace m => Int -> TileMapBaseAddress -> m [TileMap]
getTileMaps 0 tileMapAddr = do
  tileMap0 <- readTileMap tileMapAddr
  return [tileMap0]
getTileMaps 3 tileMapAddr = do
  tileMap0 <- readTileMap tileMapAddr
  tileMap1 <- readTileMap (tileMapAddr + 0x00000800)
  tileMap2 <- readTileMap (tileMapAddr + 0x00001000)
  tileMap3 <- readTileMap (tileMapAddr + 0x00001800)
  return [tileMap0, tileMap1, tileMap2, tileMap3]
getTileMaps _ tileMapAddr = do
  tileMap0 <- readTileMap tileMapAddr
  tileMap1 <- readTileMap (tileMapAddr + 0x00000800)
  return [tileMap0, tileMap1]

-- -- Text Mode
textBG :: (BGControl, TileOffset, ([TileMap], TileSet)) -> Palette -> ScreenObj
textBG (bg, offset, mapSet) palette = NormalBG bgTiles priority
  where
    bgTiles = getTextBGTiles (screenSize bg) (colorsPalettes bg) offset palette mapSet (screenBaseBlock bg) (characterBaseBlock bg)
    priority = bgPriority bg

-- THIS NEEDS TO GET THE TILES USING DRAWTILEMAP
getTextBGTiles :: Int -> PixFormat -> TileOffset -> Palette -> ([TileMap], TileSet) -> TileMapBaseAddress -> TileSetBaseAddress -> [Tile']
getTextBGTiles 0 pixFormat offSet pal (maps, tileSet) tileMapAddr tileSetAddr = bgTiles
  where
    bgTiles = concat $ mapToTileSet (32, 32) pixFormat (maps!!0) tileSet offSet pal tileMapAddr tileSetAddr
getTextBGTiles 1 pixFormat (x, y) pal (maps, tileSet) tileMapAddr tileSetAddr = bgTiles0 ++ bgTiles1
  where
    bgTiles0 = concat $ mapToTileSet (32, 32) pixFormat (maps!!0) tileSet (x, y) pal tileMapAddr tileSetAddr
    bgTiles1 = concat $ mapToTileSet (32, 32) pixFormat (maps!!1) tileSet (x+32, y) pal tileMapAddr tileSetAddr
getTextBGTiles 2 pixFormat (x, y) pal (maps, tileSet) tileMapAddr tileSetAddr = bgTiles0 ++ bgTiles1
  where
    bgTiles0 = concat $ mapToTileSet (32, 32) pixFormat (maps!!0) tileSet (x, y) pal tileMapAddr tileSetAddr
    bgTiles1 = concat $ mapToTileSet (32, 32) pixFormat (maps!!1) tileSet (x, y+32) pal tileMapAddr tileSetAddr
getTextBGTiles _ pixFormat (x, y) pal (maps, tileSet) tileMapAddr tileSetAddr = bgTiles0 ++ bgTiles1 ++ bgTiles2 ++ bgTiles3
  where
    bgTiles0 = concat $ mapToTileSet (32, 32) pixFormat (maps!!0) tileSet (x, y) pal tileMapAddr tileSetAddr
    bgTiles1 = concat $ mapToTileSet (32, 32) pixFormat (maps!!1) tileSet (x+32, y) pal tileMapAddr tileSetAddr
    bgTiles2 = concat $ mapToTileSet (32, 32) pixFormat (maps!!2) tileSet (x, y+32) pal tileMapAddr tileSetAddr
    bgTiles3 = concat $ mapToTileSet (32, 32) pixFormat (maps!!3) tileSet (x+32, y+32) pal tileMapAddr tileSetAddr

readTileMap :: AddressSpace m => Address -> m (TileMap)
readTileMap addr = readRange (addr, addr + 0x000007FF)

readCharBlocks :: AddressSpace m => Address -> PixFormat -> m TileSet
readCharBlocks addr False = readRange (addr, addr + 0x00007FFF)
readCharBlocks addr True = readRange (addr, addr + 0x0000FFFF)

-- Draw 32x32 tiles at a time
mapToTileSet :: (Int, Int) -> PixFormat -> TileMap -> TileSet -> TileOffset -> Palette -> TileMapBaseAddress -> TileSetBaseAddress -> [[Tile']]
mapToTileSet (0, _) _ _ _ _ _ _ _ = []
mapToTileSet (rows, cols) pixFormat tileMap tileSet bgOffset@(xOff, yOff) palette baseAddr setBaseAddr = row:mapToTileSet (rows-1, cols) pixFormat tileMap tileSet (xOff, yOff + 8) palette (baseAddr + 0x00000040) setBaseAddr
  where
    row = mapRow cols baseAddr pixFormat tileMapRow tileSet bgOffset palette setBaseAddr
    tileMapRow = ixmap (baseAddr, baseAddr + 0x0000003F) (id) tileMap :: TileMap

-- Need to recurse using int instead
mapRow :: Int -> Address -> PixFormat -> TileMap -> TileSet -> TileOffset -> Palette -> TileSetBaseAddress -> [Tile']
mapRow 0 _ _ _ _ _ _ _ = []
mapRow column mapIndex pixFormat tileMapRow tileSet (xOff, yOff) palette setBaseAddr =
  Tile' pixData tileCoords:mapRow (column-1) (mapIndex + 0x00000002) pixFormat tileMapRow tileSet (xOff + 8, yOff) palette setBaseAddr
  where
    pixData = pixelData' pixFormat palette tile palBank
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

-- what will be returned (BGControl, AffineRefPoints, AffineParameters, ([TileMap], TileSet))
readAffineBG :: AddressSpace m => Address -> Address -> Address -> m ()
readAffineBG bgCNTAddr refBaseAddr paramBaseAddr = do
  bg <- recordBGControl bgCNTAddr
  _xWord <- readAddressWord refBaseAddr
  _yWord <- (readAddressWord (refBaseAddr + 0x00000004))
  _paramMem <- readRange (paramBaseAddr, paramBaseAddr + 0x00000007)
  let _size = affineBGSize (screenSize bg)
  return ()

-- Returns number of tiles to be drawn
affineBGSize :: Int -> (Int, Int)
affineBGSize n
  | n == 0 = (16, 16)
  | n == 1 = (32, 32)
  | n == 2 = (64, 64)
  | otherwise = (128, 128)
