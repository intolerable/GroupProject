module Emulator.ROM.Parser
  ( readROM
  , parseROM ) where

import Emulator.ROM

import Control.Applicative
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import Prelude
import qualified Data.ByteString.Lazy as BS

-- Given a filename, return the data and header of the ROM, or an error.
readROM :: String -> IO (Either String (ROMHeader, ByteString))
readROM path = do
  parseROM <$> BS.readFile path

parseROM :: ByteString -> Either String (ROMHeader, ByteString)
parseROM str =
  case runGetOrFail parseHeader str of
    Left (_, _, err) -> Left err
    Right (rest, _, rh) -> Right (rh, rest)

parseHeader :: Get ROMHeader
parseHeader =
  ROMHeader <$> getByteString 4 -- branch instruction
            <*> getByteString 156 -- Nintendo logo
            <*> getByteString 12 -- game's title
            <*> getByteString 4 -- game's code
            <*> getByteString 2 -- maker's code
            <*> (getWord8 <* skip (13 + 36)) -- magic byte, then skip reserved (13) and multiboot (36) bytes
