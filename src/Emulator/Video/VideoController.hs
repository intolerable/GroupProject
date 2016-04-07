module Emulator.Video.VideoController where

import Emulator.Memory
import Emulator.Types
import Utilities.Parser.TemplateHaskell
import Data.Bits

data LCDControl = -- R/W
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

data LCDStatus =  -- R/W
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

data Background = BG0 | BG1 | BG2 | BG3
  deriving (Show, Read, Eq)

data BGControl (bg :: Background) =       -- R/W. BGs 0-3
  BGControl { bgPriority :: Byte          -- 0 = Highest
            , characterBaseBlock :: Byte  -- =BG Tile Data. Indicates the start of tile counting
            , mosaic :: Bool
            , colorsPalettes :: Bool      -- (0=16/16, 1=256/1)
            , screenBaseBlock :: Byte
            , displayAreaFlow :: Bool     -- BG 2 & BG 3 only
            , screenSize :: Byte }
  deriving (Show, Read, Eq)

data Axis = X | Y
  deriving (Show, Read, Eq)

type X = 'X
type Y = 'Y

data BGOffset = -- W. All layers in BG mode 0 and first two layers in BG mode 1, i.e text modes
  BGOffset { xOffset :: HalfWord
           , yOffset :: HalfWord }
  deriving (Show, Read, Eq)

data LowerUpperBit = Lower | Upper -- 16 bit | 12 bit
  deriving (Show, Read, Eq)

data BGReferencePoint (axis :: Axis) (bit :: LowerUpperBit) (bg :: Background) =  -- W. For all non text modes. For scrolling the screen
  BGReferencePoint { fractProportion :: Byte
                   , intProportion :: MWord
                   , sign :: Bool }
  deriving (Show, Read, Eq)

data Parameter = A | B | C | D
  deriving (Show, Read, Eq)

data BGRotScalParam (parameter :: Parameter) (bg :: Background) = -- W. Rotation and Scaling modes. Merge this and BGRefPoint?
  BGRotScalParam { fractProport :: Byte
                 , intProport :: Byte
                 , sign' :: Bool }
  deriving (Show, Read, Eq)

data Window = WIN0 | WIN1 | WINOBJ
  deriving (Show, Read, Eq)

data WinDimension (axis :: Axis) (win :: Window) =    -- W
  WinDimension { xy2 :: Byte         -- x = Rightmost coord of window, +1. y = Bottom coord of window, +1
               , xy1 :: Byte }       -- Leftmost coord of win. Top coord of window
  deriving (Show, Read, Eq)
-- Garbage values of X2>240 or X1>X2 are interpreted as X2=240.
-- Garbage values of Y2>160 or Y1>Y2 are interpreted as Y2=160.

data WinControl (win :: Window) = -- R/W. Control inside of (WIN0 and WIN1) or (Outside windows and OBJWIN)
  WinControl { bgsEnableBits0 :: Byte
             , objEnableBit0 :: Bool
             , colSpecialEff0 :: Bool
             , bgsEnableBits1 :: Byte
             , objEnableBit1 :: Bool
             , colSpecialEff1 :: Bool }
  deriving (Show, Read, Eq)

data MosaicSize = --  W.
  MosaicSize { bgHSize :: Byte
             , bgVSize :: Byte
             , objHSize :: Byte
             , objVSize :: Byte }
  deriving (Show, Read, Eq)

data ColourControl = -- R/W
  ColourControl { bg0TargetPixel1 :: Bool
                , bg1TargetPixel1 :: Bool
                , bg2TargetPixel1 :: Bool
                , bg3TargetPixel1 :: Bool
                , objTargetPixel1 :: Bool
                , bdTargetPixel1 :: Bool
                , colSpecEffect :: Byte
                , bg0TargetPixel :: Bool
                , bg1TargetPixel :: Bool
                , bg2TargetPixel :: Bool
                , bg3TargetPixel :: Bool
                , objTargetPixel :: Bool
                , bdTargetPixel :: Bool }
  deriving (Show, Read, Eq)

data AlphaBlendCoeff =  -- W
  AlphaBlendCoeff { evaCoeff :: Byte
                  , evbCoeff :: Byte }
  deriving (Show, Read, Eq)

newtype BrightnessCoeff = BrightnessCoeff Byte

recordLCDControl :: AddressSpace m => m LCDControl
recordLCDControl = do
  hword <- readAddressHalfWord 0x04000000
  let lcdCNT = LCDControl (fromIntegral $ $(bitmask 2 0) hword)
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
  return lcdCNT

recordLCDStatus :: HalfWord -> LCDStatus
recordLCDStatus hword =
  LCDStatus (testBit hword 0)
            (testBit hword 1)
            (testBit hword 2)
            (testBit hword 3)
            (testBit hword 4)
            (testBit hword 5)
            (fromIntegral $ $(bitmask 15 8) hword)

-- Reads a mem address that points to bgcnt register
recordBGControl :: AddressSpace m => Address -> m (BGControl a)
recordBGControl addr = do
  hword <- readAddressHalfWord addr
  let bgCNT = BGControl (fromIntegral $ $(bitmask 1 0) hword)
                        (fromIntegral $ $(bitmask 3 2) hword)
                        (testBit hword 6)
                        (testBit hword 7)
                        (fromIntegral $ $(bitmask 12 8) hword)
                        (testBit hword 13)
                        (fromIntegral $ $(bitmask 15 14) hword)
  return bgCNT

recordBGOffset :: AddressSpace m => Address -> Address -> m BGOffset
recordBGOffset xAddr yAddr = do
  xHword <- readAddressHalfWord xAddr
  yHword <- readAddressHalfWord yAddr
  let bgOffset = BGOffset (fromIntegral $ $(bitmask 8 0) xHword)
                          (fromIntegral $ $(bitmask 8 0) yHword)
  return bgOffset


recordBGReferencePoint :: MWord -> BGReferencePoint a b c
recordBGReferencePoint mword =
  BGReferencePoint (fromIntegral $ $(bitmask 7 0) mword)
                   (fromIntegral $ $(bitmask 26 8) mword)
                   (testBit mword 27)

recordBGRotScalParam :: HalfWord -> BGRotScalParam a b
recordBGRotScalParam hword =
  BGRotScalParam (fromIntegral $ $(bitmask 7 0) hword)
                 (fromIntegral $ $(bitmask 14 8) hword)
                 (testBit hword 15)

recordWinDimension :: HalfWord -> WinDimension a b
recordWinDimension hword =
  WinDimension (fromIntegral $ $(bitmask 7 0) hword)
               (fromIntegral $ $(bitmask 15 8) hword)

recordWinControl :: HalfWord -> WinControl a
recordWinControl hword =
  WinControl (fromIntegral $ $(bitmask 3 0) hword)
             (testBit hword 4)
             (testBit hword 5)
             (fromIntegral $ $(bitmask 11 8) hword)
             (testBit hword 12)
             (testBit hword 13)

recordMosaicSize :: HalfWord -> MosaicSize
recordMosaicSize hword =
  MosaicSize (fromIntegral $ $(bitmask 3 0) hword)
             (fromIntegral $ $(bitmask 7 4) hword)
             (fromIntegral $ $(bitmask 11 8) hword)
             (fromIntegral $ $(bitmask 15 12) hword)

recordColourControl :: HalfWord -> ColourControl
recordColourControl hword =
  ColourControl (testBit hword 0)
                (testBit hword 1)
                (testBit hword 2)
                (testBit hword 3)
                (testBit hword 4)
                (testBit hword 5)
                (fromIntegral $ $(bitmask 7 6) hword)
                (testBit hword 8)
                (testBit hword 9)
                (testBit hword 10)
                (testBit hword 11)
                (testBit hword 12)
                (testBit hword 13)

recordAlphaBlendCoeff :: HalfWord -> AlphaBlendCoeff
recordAlphaBlendCoeff hword =
  AlphaBlendCoeff (fromIntegral $ $(bitmask 4 0) hword)
                  (fromIntegral $ $(bitmask 12 8) hword)

recordBrightnessCoeff :: HalfWord -> BrightnessCoeff
recordBrightnessCoeff hword =
  BrightnessCoeff (fromIntegral $ $(bitmask 4 0) hword)
