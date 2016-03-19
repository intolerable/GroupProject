module Emulator.Video.VideoController where

import Emulator.Types
import Utilities.Parser.TemplateHaskell
import Data.Bits

data LCDControl =
  LCDControl { bgMode :: Byte                   -- 6-7 are prohibited
             , reservedMode :: Bool             -- Potentially is not necessary. Can only be set by BIOS opcodes
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

data LCDStatus =
  LCDStatus { vBlankFlag :: Bool         -- Read only
            , hBlankFlag :: Bool         -- Read only
            , vCounterFlag :: Bool       -- Read only
            , vBlankIRQEnable :: Bool
            , hBlankIRQEnable :: Bool
            , vCounterIRQEnable :: Bool
            , vCountSetting :: Byte }
  deriving (Show, Read, Eq)

newtype VerticalCounter = VerticalCounter Byte  -- Read Only
  deriving (Show, Read, Eq)

data BGControl =                          -- BGs 0-3
  BGControl { bgPriority :: Byte          -- 0 = Highest
            , characterBaseBlock :: Byte  -- =BG Tile Data
            , mosaic :: Bool
            , colorsPalettes :: Bool      -- (0=16/16, 1=256/1)
            , screenBaseBlock :: Byte
            , displayAreaFlow :: Bool     -- BG 2 & BG 3 only
            , screenSize :: Byte }
  deriving (Show, Read, Eq)

data BGOffset =                     -- Exclusively Text Modes
  BGOffet { xOffset :: HalfWord
          , yOffset :: HalfWord }
  deriving (Show, Read, Eq)

recordLCDControl :: HalfWord -> LCDControl
recordLCDControl hword =
  LCDControl (fromIntegral $ $(bitmask 2 0) hword)
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

recordLCDStatus :: HalfWord -> LCDStatus
recordLCDStatus hword =
  LCDStatus (testBit hword 0)
            (testBit hword 1)
            (testBit hword 2)
            (testBit hword 3)
            (testBit hword 4)
            (testBit hword 5)
            (fromIntegral $ $(bitmask 15 8) hword)

recordBGControl :: HalfWord -> BGControl
recordBGControl hword =
  BGControl (fromIntegral $ $(bitmask 1 0) hword)
            (fromIntegral $ $(bitmask 3 2) hword)
            (testBit hword 6)
            (testBit hword 7)
            (fromIntegral $ $(bitmask 12 8) hword)
            (testBit hword 13)
            (fromIntegral $ $(bitmask 15 14) hword)

