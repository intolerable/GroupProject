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

type Attribute = HalfWord
type MappingMode = Bool
type OAM = Array Address Byte
type Size = (Int, Int)
type AffineParameters = (Double, Double, Double, Double)

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
parseObjectAttr :: AddressIO m => Array Address Byte -> OAM -> TileSet -> MappingMode -> Address -> Palette -> m ()
parseObjectAttr obj oam tileSet mapMode objAddr pal = do
  case mode of
    0 -> drawSprite size Normal offset mapMode tileIdx (pixFormat, tileSet, pal, palBank, gfx) flips affineParams
    1 -> drawSprite size Affine offset mapMode tileIdx (pixFormat, tileSet, pal, palBank, gfx) flips affineParams
    _ -> return ()
  where
    (attr0, attr1, attr2) = attributes obj objAddr
    offset = (fromIntegral $ $(bitmask 7 0) attr1, fromIntegral $ $(bitmask 7 0) attr0)
    size = spriteSize (shapeSize attr0) (shapeSize attr1)
    pixFormat = (testBit attr0 13)
    gfx = (fromIntegral $ $(bitmask 11 10) attr0) :: Integer
    tileIdx = 0x06010000 + convIntToAddr (fromIntegral $ $(bitmask 9 0) attr2 :: Int) pixFormat
    shapeSize attr = (fromIntegral $ $(bitmask 15 14) attr)
    mode = (fromIntegral $ $(bitmask 9 8) attr0) :: Int
    flips = (testBit attr1 12, testBit attr1 13) :: (Bool, Bool)
    affineBaseAddr = getAffineBaseAddr $ fromIntegral $ $(bitmask 13 9) attr1
    affineParams = objAffine affineBaseAddr oam
    palBank = convIntToAddr (fromIntegral $ $(bitmask 15 12) attr2 :: Int) False

drawSprite :: AddressIO m => Size -> ObjMode -> PixFormat -> TileSet -> TileOffset -> Attribute -> Attribute -> MappingMode -> TileSetBaseAddress -> Palette -> m ()
drawSprite (0, _) _ _ _ _ _ _ _ _ _ = return ()
drawSprite (rows, cols) objMode pixFormat tileSet offset@(x, y) attr1 attr2 mapMode tileIdx pal = do
  case objMode of
    Normal -> normalSpriteRow cols pixFormat tileSet offset attr1 attr2 tileIdx pal
    Affine -> affineSpriteRow cols pixFormat tileSet offset attr1 attr2 tileIdx pal
  drawSprite (rows - 1, cols) objMode pixFormat tileSet (x, y + 8) attr1 attr2 mapMode nextTile pal
  return ()
  where
    nextTile = nextTileIdx tileIdx cols pixFormat mapMode

normalSpriteRow :: AddressIO m => Int -> PixFormat -> TileSet -> TileOffset -> Attribute -> Attribute -> TileSetBaseAddress -> Palette -> m ()
normalSpriteRow 0 _ _ _ _ _ _ _ = return ()
normalSpriteRow cols pixFormat tileSet (xOff, yOff) attr1 attr2 tileIdx palette = do
  pixData <- pixelData pixFormat palette tile palBank
  liftIO $ drawTile pixData (xOff, xOff + 8) (yOff, yOff + 8)
  normalSpriteRow (cols - 1) pixFormat tileSet (xOff + 8, yOff) attr1 attr2 nextTile palette
  return ()
  where
    tile = getTile pixFormat tileIdx tileSet
    palBank = convIntToAddr (fromIntegral $ $(bitmask 15 12) attr2 :: Int) False
    nextTile = if pixFormat then tileIdx + 0x00000040 else tileIdx + 0x00000020

affineSpriteRow :: AddressIO m => Int -> PixFormat -> TileSet -> TileOffset -> Attribute -> Attribute -> TileSetBaseAddress -> Palette -> m ()
affineSpriteRow 0 _ _ _ _ _ _ _ = return ()
affineSpriteRow _cols pixFormat tileSet (_xOff, _yOff) _attr1 attr2 tileIdx palette = do
  _pixData <- pixelData pixFormat palette tile palBank
  return ()
  where
    tile = getTile pixFormat tileIdx tileSet
    palBank = convIntToAddr (fromIntegral $ $(bitmask 15 12) attr2 :: Int) False
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
spriteSize 0 0 = (1, 1)
spriteSize 0 1 = (2, 2)
spriteSize 0 2 = (4, 4)
spriteSize 0 3 = (8, 8)
spriteSize 1 0 = (2, 1)
spriteSize 1 1 = (4, 1)
spriteSize 1 2 = (4, 2)
spriteSize 1 3 = (8, 4)
spriteSize 2 0 = (1, 2)
spriteSize 2 1 = (1, 2)
spriteSize 2 2 = (2, 4)
spriteSize _ _ = (4, 8)
