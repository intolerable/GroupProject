module Emulator.ROM where

import Emulator.Types

import Control.Lens
import Data.ByteString (ByteString)

data ROMHeader = ROMHeader
  { _romHeaderStartLoc :: ByteString -- 0x00: B <game_code_start>
  , _romHeaderNintendoLogo :: ByteString  -- 0x04: nintendo logo
  , _romHeaderGameTitle :: ByteString  -- 0xA0: Game title (uppercase ascii, max 12 bytes)
  , _romHeaderGameCode :: ByteString  -- 0xAC: Game code (uppercase ascii, max 4 bytes)
  , _romHeaderMakerCode :: ByteString  -- 0xB0: Maker code (uppercase ascii, max 2 bytes)
  , _romHeaderMagic :: Byte }    -- 0xB2: Must be 0x96
  deriving (Show, Eq)

makeFields ''ROMHeader
