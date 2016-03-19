module Emulator.Video.VideoController where

import Emulator.Types
import Utilities.Parser.TemplateHaskell
import Data.Bits

data DisplayControl =
  DisplayControl { bgMode :: Byte                   -- 6-7 are prohibited
                 , reservedMode :: Bool             -- Can only be set by BIOS opcodes
                 , displayFrameSelect :: Bool       -- bgModes 4-5 only
                 , hBlankInterval :: Bool
                 , objCharacterVRAMMapping :: Bool
                 , forcedBlank :: Bool
                 , screenDispBG0 :: Bool
                 , screenDispBG1 :: Bool
                 , screenDispBG2 :: Bool
                 , screenDispBG3 :: Bool
                 , screenDispBGOBJ :: Bool
                 , window0DispFlag :: Bool
                 , window1DispFlag :: Bool
                 , windowOBJDispFlag :: Bool }
  deriving (Show, Read, Eq)

data InterruptsStatus =
  InterruptsStatus { vBlankFlag :: Bool
                   , hBlankFlag :: Bool
                   , vCounterFlag :: Bool
                   , vBlankIRQEnable :: Bool
                   , hBlankIRQEnable :: Bool
                   , vCounterIRQEnable :: Bool
                   , vCountSetting :: Byte }
  deriving (Show, Read, Eq)

recordDisplayControl :: HalfWord -> DisplayControl
recordDisplayControl hword =
  DisplayControl (fromIntegral $ $(bitmask 2 0) hword)
                 (testBit hword 3)
                 (testBit hword 4)
                 (testBit hword 5)
                 (testBit hword 6)
                 (testBit hword 7)
                 (testBit hword 8)
                 (testBit hword 9)
                 (testBit hword 10)
                 (testBit hword 11)
                 (testBit hword 12)
                 (testBit hword 13)
                 (testBit hword 14)
                 (testBit hword 15)

recordInterruptsStatus :: HalfWord -> InterruptsStatus
recordInterruptsStatus _ = undefined
