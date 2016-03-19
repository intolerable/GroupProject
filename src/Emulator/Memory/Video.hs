module Emulator.Memory.Video where

import Emulator.Types

readVRAM :: Monad m => Address -> m HalfWord
readVRAM = undefined

readOAM :: Monad m => Address -> m HalfWord
readOAM = undefined

writeVRAM :: Monad m => Address -> HalfWord -> m ()
writeVRAM _ _ = undefined

writeOAM :: Monad m => Address -> HalfWord -> m ()
writeOAM _ _ = undefined
