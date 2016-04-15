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
  parseObjectAttr obj mapMode
  recurseOAM oam mapMode (n+1)
  where
    objAddr = 0x07000000 + 0x00000008 * (fromIntegral n)

-- Access attributes of object
parseObjectAttr :: AddressIO m => Array Address Byte -> Bool -> m ()
parseObjectAttr _obj _mapMode = undefined

attribute0 :: Byte -> Byte -> HalfWord
attribute0 _ _ = undefined

attribute1 :: Byte -> Byte -> HalfWord
attribute1 _ _ = undefined

attribute2 :: Byte -> Byte -> HalfWord
attribute2 _ _ = undefined
