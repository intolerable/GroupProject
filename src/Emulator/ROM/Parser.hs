module Emulator.ROM.Parser where

import Emulator.ROM

import Prelude hiding (drop, splitAt, head, readFile)
import Data.ByteString (ByteString, splitAt, drop, head, readFile)


readROM :: String -> IO (ROMHeader, ByteString)
readROM path = do
  contents <- readFile path
  return $ parseHeader contents


parseHeader :: ByteString -> (ROMHeader, ByteString)
parseHeader str = do
  let (branch, r1) = splitAt 4 str -- Where to jump to
  let (logo, r2) = splitAt 156 r1 -- The nintendo logo
  let (title, r3) = splitAt 12 r2 -- Game title
  let (gCode, r4) = splitAt 4 r3 -- Game code
  let (maker, r5) = splitAt 2 r4 -- Maker coder
  let (magic, r6) = splitAt 1 r5 -- magic number
  -- Skip 13 reserved bytes + 36 multiboot bytes
  let code = drop 49 r6
  (ROMHeader branch logo title gCode maker (head magic), code)

