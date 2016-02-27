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
      conditionFromByte 0 `shouldBe` Just EQ
      conditionFromByte 1 `shouldBe` Just NE
      conditionFromByte 15 `shouldBe` Nothing

    prop "fromEnum . conditionFromByte == id" $ \x ->
      case conditionFromByte x of
        Just y -> fromEnum y == fromIntegral x
        Nothing -> True
