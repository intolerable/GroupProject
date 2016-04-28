module Emulator.Video.Sprite where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Palette
import Emulator.Video.Util
import Utilities.Parser.TemplateHaskell

import Control.Monad.IO.Class
import Data.Array.IArray
import Data.Bits

data ObjMode = Normal | Affine

type AffineParameters = (Double, Double, Double, Double)
type Attribute = HalfWord
type MappingMode = Bool
type OAM = Array Address Byte
type Size = (Int, Int)
type SpriteCentre = (Double, Double)

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
parseObjectAttr :: AddressIO m => OAM -> OAM -> TileSet -> MappingMode -> Address -> Palette -> m ()
parseObjectAttr obj oam tileSet mapMode objAddr pal = do
  case mode of
    0 -> drawSprite size Normal pixFormat tileSet offset mapMode tileIdx pal palBank flips affineParams centre
    1 -> drawSprite size Affine pixFormat tileSet offset mapMode tileIdx pal palBank flips affineParams centre
    _ -> return ()
  where
    (attr0, attr1, attr2) = attributes obj objAddr
    offset = (fromIntegral $ $(bitmask 7 0) attr1, fromIntegral $ $(bitmask 7 0) attr0)
    size@(h, w) = spriteSize (shapeSize attr0) (shapeSize attr1)
    centre = (fromIntegral h*4, fromIntegral w*4) :: (Double, Double)
    pixFormat = (testBit attr0 13)
    _gfx = (fromIntegral $ $(bitmask 11 10) attr0) :: Integer
    tileIdx = 0x06010000 + convIntToAddr (fromIntegral $ $(bitmask 9 0) attr2 :: Int) pixFormat
    shapeSize attr = (fromIntegral $ $(bitmask 15 14) attr)
    mode = (fromIntegral $ $(bitmask 9 8) attr0) :: Int
    flips = (testBit attr1 12, testBit attr1 13) :: (Bool, Bool)
    affineBaseAddr = getAffineBaseAddr $ fromIntegral $ $(bitmask 13 9) attr1
    affineParams = objAffine affineBaseAddr oam
    palBank = convIntToAddr (fromIntegral $ $(bitmask 15 12) attr2 :: Int) False

drawSprite :: AddressIO m => Size -> ObjMode -> PixFormat -> TileSet -> TileOffset -> MappingMode -> TileSetBaseAddress -> Palette -> Address -> (Bool, Bool) -> AffineParameters -> SpriteCentre -> m ()
drawSprite (0, _) _ _ _ _ _ _ _ _ _ _ _ = return ()
drawSprite (rows, cols) objMode pixFormat tileSet offset@(x, y) mapMode tileIdx pal palBank flips params centre = do
  case objMode of
    Normal -> normalSpriteRow cols pixFormat tileSet offset tileIdx pal palBank flips
    Affine -> affineSpriteRow cols pixFormat tileSet offset tileIdx pal palBank params centre
  drawSprite (rows - 1, cols) objMode pixFormat tileSet (x, y + 8) mapMode nextTile pal palBank flips params centre
  return ()
  where
    nextTile = nextTileIdx tileIdx cols pixFormat mapMode

normalSpriteRow :: AddressIO m => Int -> PixFormat -> TileSet -> TileOffset -> TileSetBaseAddress -> Palette -> Address -> (Bool, Bool) -> m ()
normalSpriteRow 0 _ _ _ _ _ _ _ = return ()
normalSpriteRow cols pixFormat tileSet (xOff, yOff) tileIdx palette palBank (_hFlip, _vFlip) = do
  pixData <- pixelData pixFormat palette tile palBank
  liftIO $ drawTile pixData (xOff, xOff + 8) (yOff, yOff + 8)
  normalSpriteRow (cols - 1) pixFormat tileSet (xOff + 8, yOff) nextTile palette palBank (_hFlip, _vFlip)
  return ()
  where
    tile = getTile pixFormat tileIdx tileSet
    nextTile = if pixFormat then tileIdx + 0x00000040 else tileIdx + 0x00000020

affineSpriteRow :: AddressIO m => Int -> PixFormat -> TileSet -> TileOffset -> TileSetBaseAddress -> Palette -> Address -> AffineParameters -> SpriteCentre -> m ()
affineSpriteRow 0 _ _ _ _ _ _ _ _ = return ()
affineSpriteRow _cols pixFormat tileSet (_xOff, _yOff) tileIdx palette palBank (_pa, _pb, _pc, _pd) (_h, _w) = do
  _pixData <- pixelData pixFormat palette tile palBank
  --let x1 = (pa * (xOff - w)) + (pb * (yOff - h)) + h
  --let y1 =
  return ()
  where
    tile = getTile pixFormat tileIdx tileSet
    _nextTile = if pixFormat then tileIdx + 0x00000040 else tileIdx + 0x00000020

nextTileIdx :: TileSetBaseAddress -> Int -> PixFormat -> MappingMode -> TileSetBaseAddress
-- 1D mapping. Each row of tiles in a sprite follows on from the last in the charBlock
nextTileIdx tileIdx cols pixFormat True = tileIdx + (convIntToAddr cols pixFormat)
-- 2D mapping. Each row of tiles is at a 4000h offset from eachother
nextTileIdx tileIdx _ _ False = tileIdx + 0x00004000

getAffineBaseAddr :: Int -> Address
getAffineBaseAddr n = 0x07000006 + (0x00000008 * fromIntegral n)

objAffine :: Address -> OAM -> AffineParameters
objAffine addr oam = (pa, pb, pc, pd)
  where
    pa = convToFixedNum (oam!(addr)) (oam!(addr + 0x00000001))
    pb = convToFixedNum (oam!(addr + 0x00000008)) (oam!(addr + 0x00000009))
    pc = convToFixedNum (oam!(addr + 0x00000010)) (oam!(addr + 0x00000011))
    pd = convToFixedNum (oam!(addr + 0x00000018)) (oam!(addr + 0x00000019))

convToFixedNum :: Byte -> Byte -> Double
convToFixedNum low up
  | sign = val
  | otherwise = negate val
  where
    val =  intProp + (fracProp / 256.0)
    hword = bytesToHalfWord low up
    fracProp = fromIntegral $ $(bitmask 7 0) hword
    intProp = fromIntegral $ $(bitmask 14 8) hword
    sign = testBit hword 15

attributes :: OAM -> Address -> (Attribute, Attribute, Attribute)
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
