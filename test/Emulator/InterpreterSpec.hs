module Emulator.InterpreterSpec where

import Emulator.CPU
import Emulator.CPU.Instructions.Parser
import Emulator.Interpreter

import Control.Lens
import Test.Hspec
import qualified Data.ByteString.Lazy as ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "interpretARM" $ do

    romFile <- runIO $ ByteString.readFile "./res/suite.gba"
    let initialSystem = buildInitialState romFile

    it "should be able to handle a branch instruction" $ do
      let run instr = snd $ runIdentity $ runSystemT instr initialSystem
      run (interpretARM (Branch (Link False) 184)) ^. sysRegisters.r15 `shouldBe` 0x080000BC
      run (interpretARM (Branch (Link True) 184)) ^. sysRegisters.r14 `shouldBe` 0x08000000
