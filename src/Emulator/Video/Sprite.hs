module Emulator.Video.Sprite where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Palette
import Emulator.Video.Util

import Data.Array.IArray
import Data.Bits
import Graphics.Rendering.OpenGL
import Utilities.Parser.TemplateHaskell

type MappingMode = Bool
type OAM = Array Address Byte
type SpriteSize = (Int, Int)
type SpriteCentre = (GLdouble, GLdouble)

data SpriteAttribs =
  SpriteAttribs { getPixFormat :: PixFormat
                , getTileSet :: TileSet
                , getPal :: Palette
                , getPalBank :: Address }
  deriving (Show, Read, Eq)

readOAM :: AddressIO m => MappingMode -> m ()
readOAM mapMode = do
  oam <- readRange (0x07000000, 0x070003FF)
  tileSet <- readRange (0x06010000, 0x06017FFF)
  palette <- readRange (0x05000200, 0x050003FF)
  recurseOAM oam tileSet mapMode 0 palette
  return ()

-- Access all 128 objects in the OAM
recurseOAM :: AddressIO m => OAM -> TileSet -> MappingMode -> Int -> Palette -> m ()
recurseOAM _ _ _ 128 _ = return ()
recurseOAM oam tileSet mapMode n pal = do
  parseObjectAttr obj oam tileSet mapMode objAddr pal
  recurseOAM oam tileSet mapMode (n+1) pal
  where
    obj = ixmap (objAddr, objAddr + 0x00000005) (id) oam
    objAddr = 0x07000000 + 0x00000008 * (fromIntegral n)

-- Access attributes of object
parseObjectAttr :: OAM -> OAM -> TileSet -> MappingMode -> Address -> Palette -> ScreenObj
parseObjectAttr obj oam tileSet mapMode objAddr pal
  | mode == 0 = NormalSprite pixData flips priority
  | mode == 1 = AffineSprite (transformCoords pixData centre affineParams) priority
  | otherwise = Hidden
  where
    (attr0, attr1, attr2) = attributes obj objAddr
    mode = (fromIntegral $ $(bitmask 9 8) attr0) :: Int
    (xOff, yOff) = (fromIntegral $ $(bitmask 7 0) attr1, fromIntegral $ $(bitmask 7 0) attr0) :: (GLdouble, GLdouble)
    shapeSize attr = (fromIntegral $ $(bitmask 15 14) attr)
    size@(h, w) = spriteSize (shapeSize attr0) (shapeSize attr1)
    centre = (xOff + (fromIntegral h*4), yOff + (fromIntegral w*4)) :: (GLdouble, GLdouble)
    pixFormat = (testBit attr0 13)
    _gfx = (fromIntegral $ $(bitmask 11 10) attr0) :: Integer
    tileIdx = 0x06010000 + convIntToAddr (fromIntegral $ $(bitmask 9 0) attr2 :: Int) pixFormat
    flips = (testBit attr1 12, testBit attr1 13) :: (Bool, Bool)
    affineParams = objAffine attr1 oam
    palBank = convIntToAddr (fromIntegral $ $(bitmask 15 12) attr2 :: Int) False
    priority = (fromIntegral $ $(bitmask 11 10) attr2) :: Int
    attribs = SpriteAttribs pixFormat tileSet pal palBank
    pixData = concat $ spriteTiles size (xOff, yOff) tileIdx mapMode attribs

spriteTiles :: SpriteSize -> TileOffset -> TileSetBaseAddress -> MappingMode -> SpriteAttribs -> [[Tile']]
spriteTiles (0, _) _ _ _ _ = []
spriteTiles (rows, cols) offset@(x, y) tileIdx mapMode attribs = row:spriteTiles (rows - 1, cols) (x, y + 8) nextTile mapMode attribs
  where
    row = spriteRow cols offset tileIdx attribs
    nextTile = nextTileIdx tileIdx cols (getPixFormat attribs) mapMode

spriteRow :: Int -> TileOffset -> TileSetBaseAddress -> SpriteAttribs -> [Tile']
spriteRow 0 _ _ _ = []
spriteRow cols (xOff, yOff) tileIdx attribs = Tile' pixData tileCoords:spriteRow (cols - 1) (xOff + 8, yOff) nextTile attribs
  where
    pixData = pixelData' pixFormat (getPal attribs) tile (getPalBank attribs)
    tile = getTile pixFormat tileIdx (getTileSet attribs)
    nextTile = if pixFormat then tileIdx + 0x00000040 else tileIdx + 0x00000020
    tileCoords = ((xOff, yOff), (xOff+8, yOff), (xOff, yOff+8), (xOff+8, yOff+8))
    pixFormat = getPixFormat attribs

nextTileIdx :: TileSetBaseAddress -> Int -> PixFormat -> MappingMode -> TileSetBaseAddress
-- 1D mapping. Each row of tiles in a sprite follows on from the last in the charBlock
nextTileIdx tileIdx cols pixFormat True = tileIdx + (convIntToAddr cols pixFormat)
-- 2D mapping. Each row of tiles is at a 4000h offset from eachother
nextTileIdx tileIdx _ _ False = tileIdx + 0x00004000

getAffineBaseAddr :: Int -> Address
getAffineBaseAddr n = 0x07000006 + (0x00000008 * fromIntegral n)

objAffine :: HalfWord -> OAM -> AffineParameters
objAffine attr1 oam = params
  where
    addr = getAffineBaseAddr $ fromIntegral $ $(bitmask 13 9) attr1
    params = affineParameters addr (addr + 0x00000008) (addr + 0x00000010) (addr + 0x00000018) oam

attributes :: OAM -> Address -> (HalfWord, HalfWord, HalfWord)
attributes obj objAddr = (attr0, attr1, attr2)
  where
    attr0 = bytesToHalfWord (obj!objAddr) (obj!objAddr + 0x00000001)
    attr1 = bytesToHalfWord (obj!objAddr + 0x00000002) (obj!objAddr + 0x00000003)
    attr2 = bytesToHalfWord (obj!objAddr + 0x00000004) (obj!objAddr + 0x00000005)

spriteSize :: Byte -> Byte -> (Int, Int)
spriteSize 0 0 = (1, 1)    -- 8x8
spriteSize 0 1 = (2, 2)   -- 16x16
spriteSize 0 2 = (4, 4) -- 32x32
spriteSize 0 3 = (8, 8) -- 64x64
spriteSize 1 0 = (2, 1) -- 16x8
spriteSize 1 1 = (4, 1) -- 32x8
spriteSize 1 2 = (4, 2) -- 32x16
spriteSize 1 3 = (8, 4) -- 64x32
spriteSize 2 0 = (1, 2) -- 8x16
spriteSize 2 1 = (1, 2) -- 8x32
spriteSize 2 2 = (2, 4) -- 16x32
spriteSize _ _ = (4, 8) -- 32x64
