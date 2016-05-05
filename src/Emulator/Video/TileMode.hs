module Emulator.Video.TileMode where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Palette
import Emulator.Video.Util
import Emulator.Video.VideoController
import Utilities.Parser.TemplateHaskell

import Data.Array.IArray
import Data.Bits

type ScreenEntry = (Address, Bool, Bool, Address)
type TileMap = Array Address Byte

tileModes :: AddressIO m => LCDControl -> m [ScreenObj]
tileModes cnt = do
  palette <- readRange (0x05000000, 0x050001FF)
  case bgMode cnt of
    0 -> mode0 palette cnt
    1 -> mode1 palette cnt
    _ -> mode2 palette cnt

mode0 :: AddressSpace m => Palette -> LCDControl -> m [ScreenObj]
mode0 palette _ = do
  bg0Data <- readTextBG 0x04000008 0x04000010 0x04000012
  bg1Data <- readTextBG 0x0400000A 0x04000014 0x04000016
  bg2Data <- readTextBG 0x0400000C 0x04000018 0x0400001A
  bg3Data <- readTextBG 0x0400000E 0x0400001C 0x0400001E
  let bg0 = textBG bg0Data palette
  let bg1 = textBG bg1Data palette
  let bg2 = textBG bg2Data palette
  let bg3 = textBG bg3Data palette
  return [bg0, bg1, bg2, bg3]

mode1 :: AddressIO m => Palette -> LCDControl -> m [ScreenObj]
mode1 palette _ = do
  bg0Data <- readTextBG 0x04000008 0x04000010 0x04000012
  bg1Data <- readTextBG 0x0400000A 0x04000014 0x04000016
  bg2Data <- readAffineBG 0x0400000C 0x04000028 0x04000020
  let bg0 = textBG bg0Data palette
  let bg1 = textBG bg1Data palette
  let bg2 = affineBG bg2Data palette
  return [bg0, bg1, bg2]

mode2 :: AddressIO m => Palette -> LCDControl -> m [ScreenObj]
mode2 palette _ = do
  bg2Data <- readAffineBG 0x0400000C 0x04000028 0x04000020
  bg3Data <- readAffineBG 0x0400000E 0x04000038 0x04000030
  let bg2 = affineBG bg2Data palette
  let bg3 = affineBG bg3Data palette
  return [bg2, bg3]

readTextBG :: AddressSpace m => Address -> Address -> Address -> m (BGControl, TileOffset, ([TileMap], TileSet))
readTextBG bgCNTAddr xOffAddr yOffAddr = do
  bg <- recordBGControl bgCNTAddr
  xHWord <- readAddressHalfWord xOffAddr
  yHWord <- readAddressHalfWord yOffAddr
  let xOff = negate (fromIntegral $ $(bitmask 8 0) xHWord)
  let yOff = negate (fromIntegral $ $(bitmask 8 0) yHWord)
  tileSet <- readCharBlocks (characterBaseBlock bg) (colorsPalettes bg)
  tileMapSets <- getTextTileMaps (screenSize bg) (screenBaseBlock bg)
  return (bg, (xOff, yOff), (tileMapSets, tileSet))

getTextTileMaps :: AddressSpace m => Int -> TileMapBaseAddress -> m [TileMap]
getTextTileMaps 0 tileMapAddr = do
  tileMap0 <- readTileMap tileMapAddr
  return [tileMap0]
getTextTileMaps 3 tileMapAddr = do
  tileMap0 <- readTileMap tileMapAddr
  tileMap1 <- readTileMap (tileMapAddr + 0x00000800)
  tileMap2 <- readTileMap (tileMapAddr + 0x00001000)
  tileMap3 <- readTileMap (tileMapAddr + 0x00001800)
  return [tileMap0, tileMap1, tileMap2, tileMap3]
getTextTileMaps _ tileMapAddr = do
  tileMap0 <- readTileMap tileMapAddr
  tileMap1 <- readTileMap (tileMapAddr + 0x00000800)
  return [tileMap0, tileMap1]

-- -- Text Mode
textBG :: (BGControl, TileOffset, ([TileMap], TileSet)) -> Palette -> ScreenObj
textBG (bg, offset, mapSet) palette = NormalBG bgTiles priority
  where
    bgTiles = getTextBGTiles (screenSize bg) (colorsPalettes bg) offset palette mapSet (screenBaseBlock bg) (characterBaseBlock bg)
    priority = bgPriority bg

affineBG :: (BGControl, AffineRefPoints, AffineParameters, (Int, Int), TileMap, TileSet, Centre) -> Palette -> ScreenObj
affineBG (bg, refPoint, param, size, tileMap, tileSet, centre) pal = AffineBG affineBgTiles priority
  where
    bgTiles = concat $ mapToTileSet size (colorsPalettes bg) tileMap tileSet refPoint pal (screenBaseBlock bg) (characterBaseBlock bg)
    priority = bgPriority bg
    affineBgTiles = transformCoords bgTiles centre param

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
mapToTileSet (rows, cols) pixFormat tileMap tileSet bgOffset@(xOff, yOff) palette baseAddr setBaseAddr = row:mapToTileSet (rows-1, cols) pixFormat tileMap tileSet (xOff, yOff + 8) palette (baseAddr + tileMapRowWidth) setBaseAddr
  where
    row = mapRow cols baseAddr pixFormat tileMapRow tileSet bgOffset palette setBaseAddr
    tileMapRow = ixmap (baseAddr, baseAddr + (tileMapRowWidth - 0x00000001)) (id) tileMap :: TileMap
    tileMapRowWidth = (fromIntegral cols) * 0x00000002

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

readAffineBG :: AddressSpace m => Address -> Address -> Address -> m (BGControl, AffineRefPoints, AffineParameters, (Int, Int), TileMap, TileSet, Centre)
readAffineBG bgCNTAddr refBaseAddr paramBaseAddr = do
  bg <- recordBGControl bgCNTAddr
  xWord <- readAddressWord refBaseAddr
  yWord <- (readAddressWord (refBaseAddr + 0x00000004))
  paramMem <- readRange (paramBaseAddr, paramBaseAddr + 0x00000007)
  let refPoint@(x, y) = (referencePoint xWord, referencePoint yWord)
  let params = affineParameters paramBaseAddr (paramBaseAddr + 0x00000002) (paramBaseAddr + 0x00000004) (paramBaseAddr + 0x00000006) paramMem
  let size@(w, h) = affineBGSize (screenSize bg)
  let centre = (x + ((fromIntegral w)/2), y + ((fromIntegral h)/2))
  tileSet <- readCharBlocks (characterBaseBlock bg) (colorsPalettes bg)
  let mapSize = fromIntegral ((w * h * 2) - 1) :: Address
  tileMap <- readRange ((screenBaseBlock bg), (screenBaseBlock bg) + mapSize)
  return (bg, refPoint, params, size, tileMap, tileSet, centre)

transformCoords :: [Tile'] -> Centre -> AffineParameters -> [Tile']
transformCoords [] _ _ = []
transformCoords ((Tile' pix coords):xs) centre params = Tile' pix affCoords:transformCoords xs centre params
  where
    affCoords = affineCoords' centre params coords

-- Returns number of tiles to be drawn
affineBGSize :: Int -> (Int, Int)
affineBGSize n
  | n == 0 = (16, 16)
  | n == 1 = (32, 32)
  | n == 2 = (64, 64)
  | otherwise = (128, 128)
