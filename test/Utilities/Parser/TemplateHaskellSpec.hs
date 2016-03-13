module Utilities.Parser.TemplateHaskellSpec where

import Test.Hspec

import Utilities.Parser.TemplateHaskell

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "bitmask" $ do
    it "should be able to generate a basic bitmask" $ do
      $(bitmask 1 0) (0b1001 :: Word) `shouldBe` 0b01
      $(bitmask 3 2) (0b1001 :: Word) `shouldBe` 0b10
      $(bitmask 3 0) (0b01011010 :: Word) `shouldBe` 0b1010
