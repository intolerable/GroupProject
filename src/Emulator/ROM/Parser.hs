module Emulator.ROM.Parser (readROM) where

import Emulator.ROM

import Prelude hiding (readFile)
import Data.ByteString
import Data.Binary.Strict.Get
import Data.Word


-- Given a filename, return the data and header of the ROM, or an error.
readROM :: String -> IO (Either String (ROMHeader, ByteString))
readROM path = do
  contents <- readFile path
  return $ parseROM contents


parseROM :: ByteString -> Either String (ROMHeader, ByteString)
parseROM str =
  case runGet parseHeader str of
    (Right (branch, logo, title, gCode, maker, magic), remainder) ->
      Right (ROMHeader branch logo title gCode maker magic, remainder)
    ((Left err, _)) ->
      Left $ "ROM parsing error: " ++ err

parseHeader :: Get (ByteString, ByteString, ByteString, ByteString, ByteString, Word8)
parseHeader = do
  branch   <- getByteString 4   -- Branch instruction
  logo     <- getByteString 156 -- Nintendo logo
  title    <- getByteString 12  -- The game title
  gameCode <- getByteString 4   -- The game's code
  maker    <- getByteString 2   -- The maker code
  magic    <- getWord8          -- The magic number
  -- Skip past 13+36 reserved+multiboot bytes
  skip 49
  return (branch, logo, title, gameCode, maker, magic)
