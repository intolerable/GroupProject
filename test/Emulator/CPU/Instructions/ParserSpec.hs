module Emulator.CPU.Instructions.ParserSpec where

import Emulator.CPU
import Emulator.CPU.Instructions.Parser

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "parseARM" $ do

    it "should be able to parse a data processing instruction" $ do
      --pending
      --parseARM 0xE28BA028 `shouldBe` Right (AL, DataProcessing TST (SetCondition False) (RegisterName 22) (RegisterName 20) (Right (Rotated 8 40)))
      parseARM 0xEA00007F `shouldBe` Right (AL, Branch False 508)
      parseARM 0xE59FD028 `shouldBe` Right (AL, SingleDataTransfer Pre Up Word False Load (RegisterName 15) (RegisterName 13) (Right 40))
      parseARM 0xE5810000 `shouldBe` Right (AL, SingleDataTransfer Pre Up Word False Store (RegisterName 1) (RegisterName 0) (Right   0))
      parseARM 0xE12FFF11 `shouldBe` Right (AL, BranchExchange (RegisterName 1))
      parseARM 0xE1D310B8 `shouldBe` Right (AL, HalfwordDataTransferImmediate Pre Up False Load False HalfWord (RegisterName 3) (RegisterName 1) 8)
