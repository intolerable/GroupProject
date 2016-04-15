module Emulator.Video.Sprite where

import Emulator.Memory
import Emulator.Types
import Emulator.Video.Util

import Data.Array.IArray

type MappingMode = Bool

readOAM :: AddressIO m => MappingMode -> m ()
readOAM _mapMode = do
  _oam <- readRange (0x07000000, 0x070003FF)
  return ()

-- Access each object
recurseOAM :: AddressIO m => Array Address Byte -> MappingMode -> m ()
recurseOAM _oam _mapMode = undefined

-- Access attributes of object
parseObject :: AddressIO m => Array Address Byte -> m ()
parseObject _obj = undefined

attribute0 :: Byte -> Byte -> HalfWord
attribute0 _ _ = undefined

attribute1 :: Byte -> Byte -> HalfWord
attribute1 _ _ = undefined

attribute2 :: Byte -> Byte -> HalfWord
attribute2 _ _ = undefined
