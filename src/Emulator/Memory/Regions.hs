module Emulator.Memory.Regions where

import Emulator.Types

import Data.Ix
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

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

regions :: Map Address RegionType
regions = Map.fromList [ (0x00000000, BIOS),
            (0x00004000, Unused),
            (0x02000000, WRAM),
            (0x02040000, Unused),
            (0x03000000, WRAM),
            (0x03008000, Unused),
            (0x04000000, IORegisters),
            (0x04000400, Unused),
            (0x05000000, PaletteRAM),
            (0x05000400, Unused),
            (0x06000000, VRAM),
            (0x06018000, Unused),
            (0x07000000, ObjAttributes),
            (0x07000400, Unused),
            (0x08000000, GamePakROM),
            (0x0E000000, GamePakSRAM),
            (0x0E010000, Unused) ]

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
addressToRegionType addr =
	case Map.lookupLE addr regions of
		Just (_, x) -> x
		Nothing -> error "Unrecognized region"

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
