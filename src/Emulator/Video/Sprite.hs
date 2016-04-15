module Emulator.Video.Sprite where

import Emulator.Memory
import Emulator.Types
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
  tileSet <- readRange (0x06010000, 0x06013FFF)
  recurseOAM oam tileSet mapMode 0
  return ()

-- Access each object
recurseOAM :: AddressIO m => OAM -> TileSet -> MappingMode -> Int -> m ()
recurseOAM _ _ _ 128 = return ()
recurseOAM oam tileSet mapMode n = do
  let obj = ixmap (objAddr, objAddr + 0x00000005) (id) oam
  parseObjectAttr obj tileSet mapMode objAddr
  recurseOAM oam tileSet mapMode (n+1)
  where
    objAddr = 0x07000000 + 0x00000008 * (fromIntegral n)

-- Access attributes of object
parseObjectAttr :: AddressIO m => OAM -> TileSet -> MappingMode -> Address -> m ()
parseObjectAttr obj _tileSet mapMode objAddr = do
  let (attr0, attr1, attr2) = attributes obj objAddr
  let objMode = mode (fromIntegral $ $(bitmask 9 8) attr0)
  let offset = (fromIntegral $ $(bitmask 7 0) attr1, fromIntegral $ $(bitmask 7 0) attr0)
  let size = spriteSize (shapeSize attr0) (shapeSize attr1)
  let pixFormat = (testBit attr0 13)
  let _gfx = (fromIntegral $ $(bitmask 11 10) attr0) :: Integer
  drawSprite objMode size pixFormat offset attr1 attr2 mapMode
  return ()
  where
    shapeSize attr = (fromIntegral $ $(bitmask 15 14) attr)

drawSprite :: AddressIO m => ObjectMode -> Size -> PixFormat -> TileOffset -> Attribute -> MappingMode -> m ()
drawSprite Normal _size _pixFormat _offset attr _mapMode = do
  let (_hFlip, _vFlip) = (testBit attr 12, testBit attr 13) :: (Bool, Bool)
  return ()
drawSprite Affine _size _pixFormat _offset _attr _mapMode = do
  -- read affine values from attr1
  return ()
drawSprite _ _ _ _ _ _ = return ()

attributes :: OAM -> Address -> (HalfWord, HalfWord, HalfWord)
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
