module Emulator.CPU.InstructionsSpec where

import Emulator.CPU.Instructions
import Emulator.Types

import Data.Bits
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
  describe "checkCarry" $ do

    prop "x + y > maxBound == checkCarry x y" $ \x y ->
      (fromIntegral x + fromIntegral y > (0x00000000FFFFFFFF :: DWord)) == checkCarry x y

    it "should detect a set carry" $ do
     checkCarry 0 0 `shouldBe` False
     checkCarry 0 maxBound `shouldBe` False
     checkCarry maxBound 0 `shouldBe` False
     checkCarry maxBound 1 `shouldBe` True
     checkCarry maxBound maxBound `shouldBe` True

  describe "checkSign" $ do

    prop "x & 0x80000000 == checkSign x" $ \x ->
      (x .&. 0x80000000 > 0) == checkSign x

    it "should detect the sign bit" $ do
      checkSign 0 `shouldBe` False
      checkSign 1 `shouldBe` False
      checkSign 100 `shouldBe` False
      checkSign (-1) `shouldBe` True
      checkSign (-100) `shouldBe` True
      checkSign maxBound `shouldBe` True
