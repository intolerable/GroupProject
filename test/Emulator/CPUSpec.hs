module Emulator.CPUSpec where

import Emulator.CPU

import Test.Hspec
import Test.Hspec.QuickCheck
import Prelude hiding (EQ)

main :: IO ()
main = hspec spec

spec :: Spec
spec =

  describe "conditionFromWord" $ do

    it "should be able to decode basic conditions" $ do
      conditionFromWord 0 `shouldBe` Just EQ
      conditionFromWord 1 `shouldBe` Just NE
      conditionFromWord 15 `shouldBe` Nothing

    prop "fromEnum . conditionFromWord == id" $ \x ->
      case conditionFromWord x of
        Just y -> fromEnum y == fromIntegral x
        Nothing -> True
