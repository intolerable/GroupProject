module Emulator.CPU.Instructions.ParserSpec where

import Emulator.CPU hiding (SoftwareInterrupt)
import Emulator.CPU.Instructions.Parser
import Emulator.Types

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "parseARM" $ do

    it "should be able to parse a data processing instruction" $ do
      --pending
      --parseARM 0xE28BA028 `shouldBe` Right (AL, DataProcessing TST (SetCondition False) (RegisterName 22) (RegisterName 20) (Right (Rotated 8 40)))
      parseARM 0xEA00002E `shouldBe` Right (AL, Branch (Link False) 184)
      parseARM 0xEA00007F `shouldBe` Right (AL, Branch (Link False) 508)
      parseARM 0xE59FD028 `shouldBe` Right (AL, SingleDataTransfer Pre Up Word False Load (RegisterName 15) (RegisterName 13) (Right 40))
      parseARM 0xE5810000 `shouldBe` Right (AL, SingleDataTransfer Pre Up Word False Store (RegisterName 1) (RegisterName 0) (Right 0))
      parseARM 0xE12FFF11 `shouldBe` Right (AL, BranchExchange (RegisterName 1))
      parseARM 0xE1D310B8 `shouldBe` Right (AL, HalfwordDataTransferImmediate Pre Up False Load False HalfWord (RegisterName 3) (RegisterName 1) 8)
      parseARM 0xE2833C02 `shouldBe` Right (AL, DataProcessing ADD (SetCondition False) (RegisterName 3) (RegisterName 3) $ Right (Rotated 12 2))
      parseARM 0xE3A00000 `shouldBe` Right (AL, DataProcessing MOV (SetCondition False) (RegisterName 0) (RegisterName 0) $ Right (Rotated 0 0))
      parseARM 0xE2110040 `shouldBe` Right (AL, DataProcessing AND (SetCondition True) (RegisterName 1) (RegisterName 0) $ Right (Rotated 0 64))
      parseARM 0xEF000000 `shouldBe` Right (AL, SoftwareInterrupt)
