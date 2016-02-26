module Emulator.CPU.InstructionsSpec where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.Types

import Control.Lens
import Control.Monad.Trans.State
import Data.Bits
import Data.Default.Class
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "runCondition" $ do

    prop "runCondition AL ~= True" $ \x ->
      evalState (runCondition AL) (def & cpsr .~ x) == True

    it "should always run with an AL condition" $
      evalState (runCondition AL) def `shouldBe` True

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

instance Arbitrary Flags where
  arbitrary = Flags <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
  shrink = const []
