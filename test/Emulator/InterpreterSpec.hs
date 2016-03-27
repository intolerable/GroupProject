module Emulator.InterpreterSpec where

import Emulator.CPU.Instructions.Parser
import Emulator.CPU
import Emulator.Interpreter

import Control.Lens
import Control.Monad.Trans.State
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
      execState (runSystem (interpretARM (Branch (Link False) 184))) initialSystem ^. sysRegisters.r15 `shouldBe` 0x080000BC
      execState (runSystem (interpretARM (Branch (Link True) 184))) initialSystem ^. sysRegisters.r14 `shouldBe` 0x08000000

