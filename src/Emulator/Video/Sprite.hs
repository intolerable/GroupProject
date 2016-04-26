module Emulator.Video.Sprite where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Palette
import Emulator.Video.Util
import Utilities.Parser.TemplateHaskell

import Data.Array.IArray
import Data.Bits

data ObjectMode = Normal | Affine | Hide
  deriving (Show, Read, Eq)

type Attribute = HalfWord
type MappingMode = Bool
type OAM = Array Address Byte
type Size = (Int, Int)

readOAM :: AddressIO m => MappingMode -> m ()
readOAM mapMode = do
  oam <- readRange (0x07000000, 0x070003FF)
  tileSet <- readRange (0x06010000, 0x06017FFF)
  palette <- readRange (0x05000200, 0x050003FF)
  recurseOAM oam tileSet mapMode 0 palette
  return ()

-- Access each object
recurseOAM :: AddressIO m => OAM -> TileSet -> MappingMode -> Int -> Palette -> m ()
recurseOAM _ _ _ 128 _ = return ()
recurseOAM oam tileSet mapMode n pal = do
  let obj = ixmap (objAddr, objAddr + 0x00000005) (id) oam
  parseObjectAttr obj tileSet mapMode objAddr pal
  recurseOAM oam tileSet mapMode (n+1) pal
  where
    objAddr = 0x07000000 + 0x00000008 * (fromIntegral n)

-- Access attributes of object
parseObjectAttr :: AddressIO m => OAM -> TileSet -> MappingMode -> Address -> Palette -> m ()
parseObjectAttr obj tileSet mapMode objAddr pal = do
  let (attr0, attr1, attr2) = attributes obj objAddr
  let objMode = mode (fromIntegral $ $(bitmask 9 8) attr0)
  let offset = (fromIntegral $ $(bitmask 7 0) attr1, fromIntegral $ $(bitmask 7 0) attr0)
  let size = spriteSize (shapeSize attr0) (shapeSize attr1)
  let pixFormat = (testBit attr0 13)
  let _gfx = (fromIntegral $ $(bitmask 11 10) attr0) :: Integer
  let tileIdx = 0x06010000 + convIntToAddr (fromIntegral $ $(bitmask 9 0) attr2 :: Int) pixFormat
  case objMode of
    Hide -> return ()
    Normal -> drawSprite size pixFormat tileSet offset attr1 attr2 mapMode tileIdx pal
    Affine -> drawAffineSprite
  where
    shapeSize attr = (fromIntegral $ $(bitmask 15 14) attr)

drawSprite :: AddressIO m => Size -> PixFormat -> TileSet -> TileOffset -> Attribute -> Attribute -> MappingMode -> TileSetBaseAddress -> Palette -> m ()
drawSprite (0, _) _ _ _ _ _ _ _ _ = return ()
drawSprite (rows, cols) pixFormat tileSet offset@(x, y) attr1 attr2 mapMode tileIdx pal = do
  let (_hFlip, _vFlip) = (testBit attr1 12, testBit attr1 13) :: (Bool, Bool)
  drawSpriteRow cols pixFormat tileSet offset attr2 tileIdx pal
  let nextTile = nextTileIdx tileIdx cols pixFormat mapMode
  drawSprite (rows - 1, cols) pixFormat tileSet (x, y + 8) attr1 attr2 mapMode nextTile pal
  return ()

drawSpriteRow :: AddressSpace m => Int -> PixFormat -> TileSet -> TileOffset -> Attribute -> TileSetBaseAddress -> Palette -> m ()
drawSpriteRow 0 _ _ _ _ _ _ = return ()
drawSpriteRow _cols pixFormat tileSet _offset _attr2 tileIdx _palette = do
  let _tile = getTile pixFormat tileIdx tileSet
  return ()

drawAffineSprite :: AddressSpace m => m ()
drawAffineSprite = undefined

nextTileIdx :: TileSetBaseAddress -> Int -> PixFormat -> MappingMode -> TileSetBaseAddress
-- 1D mapping
nextTileIdx tileIdx cols pixFormat True = tileIdx + (convIntToAddr cols pixFormat)
-- 2D mapping
nextTileIdx tileIdx _ _ False = tileIdx + 0x00004000

attributes :: OAM -> Address -> (Attribute, Attribute, Attribute)
attributes obj objAddr = (attr0, attr1, attr2)
  where
    attr0 = bytesToHalfWord (obj!objAddr) (obj!objAddr + 0x00000001)
    attr1 = bytesToHalfWord (obj!objAddr + 0x00000002) (obj!objAddr + 0x00000003)
    attr2 = bytesToHalfWord (obj!objAddr + 0x00000004) (obj!objAddr + 0x00000005)

mode :: Byte -> ObjectMode
mode 0 = Normal
mode 2 = Hide
mode _ = Affine

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
