module Emulator.CPU.InstructionSpec where


import Emulator.CPU.Instructions
import Emulator.Types

import Test.Hspec
import Test.Hspec.QuickCheck


main :: IO ()
main = hspec spec


spec :: Spec
spec =

  describe "checkCarry" $ do
  
    prop "x + y > maxBound == checkCarry x y" $ \x y ->
      (fromIntegral x + fromIntegral y > (0x00000000FFFFFFFF :: DWord)) == checkCarry x y
      
    it "should detect a set carry" $ do
     checkCarry 0 0 `shouldBe` False
     checkCarry 0 maxBound `shouldBe` False
     checkCarry maxBound 0 `shouldBe` False
     checkCarry maxBound 1 `shouldBe` True
     checkCarry maxBound maxBound `shouldBe` True
