module Utilities.Show
  ( showBinary
  , showHex
  , showHexPadded ) where

import Data.Char
import qualified Numeric

showBinary :: (Integral a, Show a) => a -> String
showBinary x = "0b" ++ Numeric.showIntAtBase 2 (toUpper . intToDigit) x ""

showHex :: (Integral a, Show a) => a -> String
showHex x = "0x" ++ Numeric.showIntAtBase 16 (toUpper . intToDigit) x ""

showHexPadded :: (Integral a, Show a) => Int -> a -> String
showHexPadded n x = "0x" ++ replicate (n - length numStr) '0' ++ numStr
  where
    numStr = Numeric.showIntAtBase 16 (toUpper . intToDigit) x ""
