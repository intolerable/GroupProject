module Emulator.Memory.Regions where

import Emulator.Types

data RegionType = BIOS
                | Unused
                | WRAM
                | IORegisters
                | PaletteRAM
                | VRAM
                | ObjAttributes
                | GamePakROM
                | GamePakSRAM
            deriving (Show, Eq)


regions :: [(Address, Address, RegionType)]
regions = [ (0x00000000, 0x00003FFF, BIOS),
            (0x00004000, 0x01FFFFFF, Unused),
            (0x02000000, 0x0203FFFF, WRAM),
            (0x02040000, 0x02FFFFFF, Unused),
            (0x03000000, 0x03007FFF, WRAM),
            (0x03008000, 0x03FFFFFF, Unused),
            (0x04000000, 0x040003FE, IORegisters),
            (0x04000400, 0x04FFFFFF, Unused),
            (0x05000000, 0x050003FF, PaletteRAM),
            (0x05000400, 0x05FFFFFF, Unused),
            (0x06000000, 0x06017FFF, VRAM),
            (0x06018000, 0x06FFFFFF, Unused),
            (0x07000000, 0x070003FF, ObjAttributes),
            (0x07000400, 0x07FFFFFF, Unused),
            (0x08000000, 0x0DFFFFFF, GamePakROM),
            (0x0E000000, 0x0E00FFFF, GamePakSRAM),
            (0x0E010000, 0xFFFFFFFF, Unused) ]

canWrite :: RegionType -> Boolean
canWrite BIOS          = False
canWrite Unused        = False
canWrite WRAM          = True
canWrite IORegisters   = True -- Can we definitely write to these??
canWrite PaletteRAM    = True
canWrite VRAM          = True
canWrite ObjAttributes = True
canWrite GamePakROM    = False
canWrite GamePakSRAM   = True

canRead :: RegionType -> Boolean
canRead BIOS          = False
canRead Unused        = False
canRead WRAM          = True
canRead IORegisters   = True
canRead PaletteRAM    = True
canRead VRAM          = True
canRead ObjAttributes = True
canRead GamePakROM    = True
canRead GamePakSRAM   = True
