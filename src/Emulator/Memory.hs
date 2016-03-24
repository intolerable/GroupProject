module Emulator.Memory where

import Emulator.Memory.RAM
import Emulator.Memory.Region
import Emulator.Memory.Regions
import Emulator.Memory.Video
import Emulator.Types
import Data.Proxy

type AddressSpace m =
  ( Functor m, Monad m
  , CanRead WRAM m, CanWrite WRAM m )

writeAddress :: AddressSpace m => Address -> HalfWord -> m ()
writeAddress addr hw =
  case addressToRegionType addr of
    BIOS -> return ()
    WRAM -> writeHalfWord (Proxy :: Proxy WRAM) addr hw
    VRAM -> writeVRAM addr hw
    ObjAttributes -> writeOAM addr hw
    Unused -> return ()
    _ -> undefined

readAddress :: AddressSpace m => Address -> m HalfWord
readAddress addr =
  case addressToRegionType addr of
    BIOS -> return 0
    WRAM -> readHalfWord (Proxy :: Proxy WRAM) addr
    VRAM -> readVRAM addr
    ObjAttributes -> readOAM addr
    Unused -> return 0
    _ -> undefined
