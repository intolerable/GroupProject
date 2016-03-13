module Utilities.Parser.TemplateHaskell where

import Control.Monad
import Data.Bits
import Language.Haskell.TH

-- $(bitmask x y) :: (Integral a, Bits a, Num b) => a -> b
bitmask :: Word -> Word -> Q Exp
bitmask x y = do
  when (y > x) $ fail "Invalid bitmask: the lower bound for a bitmask must be less than its upper bound."
  varName <- newName "w"
  -- \w -> fromIntegral ((w .&. mask x y) `shiftR` y)
  return $ LamE [VarP varName] $
    VarE 'fromIntegral $$ InfixE (Just $ InfixE (Just $ VarE varName) (VarE '(.&.)) (Just $ LitE $ IntegerL $ fromIntegral mask)) (VarE 'shiftR) (Just $ LitE $ IntegerL $ fromIntegral y)
  where
    ($$) = AppE
    a, b :: Word
    a = 1 `shiftL` fromIntegral x
    b = 1 `shiftL` fromIntegral y
    mask :: Word
    mask = ((a - 1) .&. complement (b - 1)) .|. a
