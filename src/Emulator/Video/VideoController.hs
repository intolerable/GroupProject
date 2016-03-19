module Emulator.Video.VideoController where

import Emulator.Memory.Video
import Emulator.Types
import Utilities.Parser.TemplateHaskell

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

readDisplayControl :: HalfWord -> DisplayControl
readDisplayControl _ = undefined
