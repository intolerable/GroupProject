module Emulator.Video.VideoController where

import Emulator.Memory.Video
import Emulator.Types
import Utilities.Parser.TemplateHaskell
import Data.Bits

data DisplayControl =
  DisplayControl { bgMode :: Byte
                 , reservedMode :: Bool
                 , displayFrameSelect :: Bool
                 , hblankInterval :: Bool
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

