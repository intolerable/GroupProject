module Emulator.Memory where

import Emulator.Memory.RAM
import Emulator.Memory.Regions
import Emulator.Memory.Video
import Emulator.Types

type AddressSpace m = (Functor m, Monad m, Mem m)

writeAddress :: AddressSpace m => Address -> HalfWord -> m ()
writeAddress addr hw =
  case addressToRegionType addr of
    BIOS -> return ()
    WRAM -> writeHalfWordLE addr hw
    VRAM -> writeVRAM addr hw
    ObjAttributes -> writeOAM addr hw
    Unused -> return ()
    _ -> undefined

readAddress :: AddressSpace m => Address -> m HalfWord
readAddress addr =
  case addressToRegionType addr of
    BIOS -> return 0
    WRAM -> readHalfWordLE addr
    VRAM -> readVRAM addr
    ObjAttributes -> readOAM addr
    Unused -> return 0
    _ -> undefined
