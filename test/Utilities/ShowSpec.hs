module Utilities.ShowSpec where

import Test.Hspec

import Utilities.Show

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "showBinary" $ do

    it "should be able to print a number as a binary literal" $ do
      let sb = showBinary :: Int -> String
      sb 0b1011 `shouldBe` "0b1011"
      sb 0b10101010 `shouldBe` "0b10101010"

  describe "showHex" $ do

    it "should be able to print a number as a hex literal" $ do
      let sh = showHex :: Int -> String
      sh 0xFF `shouldBe` "0xFF"
      sh 0xFE08 `shouldBe` "0xFE08"
