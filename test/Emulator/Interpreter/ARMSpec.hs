module Emulator.Interpreter.ARMSpec where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.Interpreter.ARM
import Emulator.Interpreter.Monad
import Emulator.Types

import Control.Lens
import Test.Hspec
import qualified Data.ByteString.Lazy as ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "interpretARM" $ do

    context "Branch" $ do

      system "should be able to branch" (0x00000024, 0x00000000) $ do
        registers.pc .= 0x00000004
        registers.pc += 4
        interpretARM $ Branch (Link False) 24
        (,) <$> use (registers.pc) <*> use (registers.lr)

      system "should be able to branch with link" (0x00000024, 0x00000004) $ do
        registers.pc .= 0x00000004
        registers.pc += 4
        interpretARM $ Branch (Link True) 24
        (,) <$> use (registers.pc) <*> use (registers.lr)

      system "should be able to branch with link, then branch exchange back" 0x00000008 $ do
        registers.pc .= 0x00000004
        registers.pc += 4
        interpretARM $ Branch (Link True) 24
        registers.pc += 4
        interpretARM $ BranchExchange (RegisterName 14)
        use (registers.pc)

system :: (Show a, Eq a) => String -> a -> SystemT Identity a -> Spec
system label val act = do
  romFile <- runIO $ ByteString.readFile "./res/suite.gba"
  it label $
    fst (runIdentity (runSystemT act (buildInitialState romFile ByteString.empty))) `shouldBe` val
