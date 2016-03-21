module Utilities.Show
  ( showBinary
  , showHex ) where

import Data.Char
import qualified Numeric

showBinary :: (Integral a, Show a) => a -> String
showBinary x = "0b" ++ Numeric.showIntAtBase 2 (toUpper . intToDigit) x ""

showHex :: (Integral a, Show a) => a -> String
showHex x = "0x" ++ Numeric.showIntAtBase 16 (toUpper . intToDigit) x ""
