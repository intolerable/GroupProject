module Emulator.ROM where

import Emulator.Types

import Control.Lens
import Data.ByteString (ByteString)

data ROMHeader = ROMHeader
  deriving (Show, Eq)

makeFields ''ROMHeader
