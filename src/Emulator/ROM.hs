module Emulator.ROM where

import Emulator.Types

import Data.ByteString (ByteString)

data ROMHeader = ROMHeader {
  -- Record                  -- Offset
  rh_startLoc        :: ByteString, -- 0x00: B <game_code_start>
  rh_nintendoLogo    :: ByteString,  -- 0x04: nintendo logo
  rh_gameTitle       :: ByteString,  -- 0xA0: Game title (uppercase ascii, max 12 bytes)
  rh_gameCode        :: ByteString,  -- 0xAC: Game code (uppercase ascii, max 4 bytes)
  rh_makerCode       :: ByteString,  -- 0xB0: Maker code (uppercase ascii, max 2 bytes)
  rh_magic           :: Byte    -- 0xB2: Must be 0x96
} deriving Show
