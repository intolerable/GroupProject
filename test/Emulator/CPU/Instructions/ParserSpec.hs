module Emulator.CPU.Instructions.ParserSpec where

import Emulator.CPU.Instructions.Parser

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "parseARM" $ do

    it "should be able to parse a data processing instruction" $
      pending
      -- parseARM 0xE28BA028 `shouldBe` Right (AL, DataProcessing TST (SetCondition False) (RegisterName 22) (RegisterName 20) (Right (Rotated 8 40)))
