module Emulator.Video.Sprite where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Util

import Data.Array.IArray

type MappingMode = Bool
type OAM = Array Address Byte

readOAM :: AddressIO m => MappingMode -> m ()
readOAM mapMode = do
  oam <- readRange (0x07000000, 0x070003FF)
  recurseOAM oam mapMode 0
  return ()

-- Access each object
recurseOAM :: AddressIO m => OAM -> MappingMode -> Int -> m ()
recurseOAM _ _ 128 = return ()
recurseOAM oam mapMode n = do
  let obj = ixmap (objAddr, objAddr + 0x00000005) (id) oam
  parseObjectAttr obj mapMode objAddr
  recurseOAM oam mapMode (n+1)
  where
    objAddr = 0x07000000 + 0x00000008 * (fromIntegral n)

-- Access attributes of object
parseObjectAttr :: AddressIO m => OAM -> MappingMode -> Address -> m ()
parseObjectAttr obj _mapMode objAddr = do
  let (_attr0, _attr1, _attr2) = attributes obj objAddr
  return ()

attributes :: OAM -> Address -> (HalfWord, HalfWord, HalfWord)
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
