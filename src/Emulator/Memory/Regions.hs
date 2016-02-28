module Emulator.Memory.Regions where

import Emulator.Types

import Data.Ix

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

type Region = (Address, Address, RegionType)

regions :: [Region]
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

canWrite :: RegionType -> Bool
canWrite BIOS          = False
canWrite Unused        = False
canWrite WRAM          = True
canWrite IORegisters   = True -- Can we definitely write to these??
canWrite PaletteRAM    = True
canWrite VRAM          = True
canWrite ObjAttributes = True
canWrite GamePakROM    = False
canWrite GamePakSRAM   = True

canRead :: RegionType -> Bool
canRead BIOS          = False
canRead Unused        = False
canRead WRAM          = True
canRead IORegisters   = True
canRead PaletteRAM    = True
canRead VRAM          = True
canRead ObjAttributes = True
canRead GamePakROM    = True
canRead GamePakSRAM   = True

addressToRegionType :: Address -> RegionType
addressToRegionType = addrToRegion regions
  where
    addrToRegion :: [Region] -> Address -> RegionType
    addrToRegion [] _ = error "Cannot find type for address. This shouldn't happen."
    addrToRegion (r:anges) addr = if inRange (rangeMin, rangeMax) addr then regionType else addrToRegion anges addr
      where (rangeMin, rangeMax, regionType) = r

addressPermissions :: Address -> (Bool, Bool)
addressPermissions addr = (canRead regionType, canWrite regionType)
  where
    regionType = addressToRegionType addr

data DispRegisters = DispCntrl
                      | GreenSwap
                      | DispStatus
                      | VCount
                      | BG0Cntrl
                      | BG1Cntrl
                      | BG2Cntrl
                      | BG3Cntrl
                      | BG0HOffset
                      | BG0VOffset
                      | BG1HOffset
                      | BG1VOffset
                      | BG2HOffset
                      | BG2VOffset
                      | BG3HOffset
                      | BG3VOffset
                      | BG2RotScalPA
                      | BG2RotScalPB
                      | BG2RotScalPC
                      | BG2RotScalPD
                      | BG2RefPointX
                      | BG2RefPointY
                      | BG3RotScalPA
                      | BG3RotScalPB
                      | BG3RotScalPC
                      | BG3RotScalPD
                      | BG3RefPointX
                      | BG3RefPointY
                      | WIN0H     -- Window 0 Horizontal Dimensions
                      | WIN1H     -- Window 1 Horizontal Dimensions
                      | WIN0V     -- Window 0 Vertical Dimensions
                      | WIN1V     -- Window 1 Vertical Dimensions
                      | WININ     -- Inside of Window 0 and 1
                      | WINOUT    -- Inside of OBJ Window & Outside of Windows
                      | MOSAIC
                      | NotUsed   -- Does this need to be mentioned?
                      | BLDCNT    -- Color Special Effects Selection
                      | BLGALPHA  -- Alpha Blending Coefficients
                      | BLDY      -- Brightness Coefficient
                deriving (Show, Read, Eq)
