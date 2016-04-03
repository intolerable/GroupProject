module Utilities.Parser.TemplateHaskell where

import Control.Monad
import Data.Bits
import Language.Haskell.TH

-- | @$(bitmask x y) :: (Integral a, Bits a, Num a) => a -> a@
--
--   @$(bitmask x y)@ generates a function which masks a given bitstring and returns the bits whose
--     indexes are between @x@ and @y@. It is a compile error for @y@ to be bigger than @x@.
bitmask :: Word -> Word -> Q Exp
bitmask x y = do
  when (y > x) $ fail "Invalid bitmask: the lower bound for a bitmask must be less than its upper bound."
  varName <- newName "w"
  -- \w -> fromIntegral ((w .&. mask x y) `shiftR` y)
  return $ LamE [VarP varName] $
    InfixE (Just $ InfixE (Just $ VarE varName) (VarE '(.&.)) (Just $ LitE $ IntegerL $ fromIntegral mask)) (VarE 'shiftR) (Just $ LitE $ IntegerL $ fromIntegral y)
  where
    a, b :: Word
    a = 1 `shiftL` fromIntegral x
    b = 1 `shiftL` fromIntegral y
    mask :: Word
    mask = ((a - 1) .&. complement (b - 1)) .|. a
