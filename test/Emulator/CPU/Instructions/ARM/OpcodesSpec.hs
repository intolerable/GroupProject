module Emulator.CPU.Instructions.ARM.OpcodesSpec where

import Emulator.CPU.Instructions.ARM.Opcodes
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

  describe "isNegative" $ do

    prop "x & 0x80000000 == isNegative x" $ \x ->
      (x .&. 0x80000000 > 0) == isNegative x

    it "should detect the sign bit" $ do
      isNegative 0 `shouldBe` False
      isNegative 1 `shouldBe` False
      isNegative 100 `shouldBe` False
      isNegative (-1) `shouldBe` True
      isNegative (-100) `shouldBe` True
      isNegative maxBound `shouldBe` True
