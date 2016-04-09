module Emulator.CPU.InstructionsSpec where

import Emulator.CPU
import Emulator.CPU.Instructions

import Control.Monad.Trans.State
import Data.Default.Class
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "runCondition" $ do

    prop "runCondition AL ~= True" $ \(x :: Flags) ->
      evalState (runCondition AL) x == True

    it "should always run with an AL condition" $
      evalState (runCondition AL) (def :: Flags) `shouldBe` True

instance Arbitrary Flags where
  arbitrary = mkFlags <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
  shrink = const []
