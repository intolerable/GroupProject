module Emulator.ROM where

import Emulator.Types

data ROMHeader = RomHeader {
  -- Record                  -- Offset
  rh_branchInstr     :: Address, -- 0x00: B <game_code_start>
  rh_nintendoLogo    :: [Byte],  -- 0x04: nintendo logo
  rh_gameTitle       :: String,  -- 0xA0: Game title (uppercase ascii, max 12 bytes)
  rh_gameCode        :: String,  -- 0xAC: Game code (uppercase ascii, max 4 bytes)
  rh_makerCode       :: String,  -- 0xB0: Maker code (uppercase ascii, max 2 bytes)
  rh_magic           :: Byte,    -- 0xB2: Must be 0x96
  rh_unitCode        :: Byte,    -- 0xB3: Probably will be 0
  rh_deviceType      :: Byte,    -- 0xB4: Probably 0
  rh_reserved        :: [Byte],  -- 0xB5: Reserved area of 7 bytes, should be zero'd
  rh_swVersion       :: Byte,    -- 0xBC: Software version
  rh_complementCheck :: Byte,    -- 0xBD: Complement check,
  rh_reserved2       :: [Byte]   -- 0xBE: Reserved zero'd 2 bytes
  -- Multiboot information goes here which shouldn't be needed,
  -- (at least yet)
}
