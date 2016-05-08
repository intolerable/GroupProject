module Emulator.Interpreter.THUMBSpec where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.Interpreter.Monad
import Emulator.Interpreter.THUMB
import Emulator.Memory

import Control.Lens
import Test.Hspec
import Prelude hiding (Ordering(..))
import qualified Data.ByteString.Lazy as ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "interpretThumb" $ do

    context "ConditionalBranch" $ do
      system "can manage a basic conditional branch" 0x0807AE6A $ do
        registers.pc .= 0x0807AE54
        registers.pc += 2
        flags.zero .= True
        interpretThumb $ ConditionalBranch EQ 18
        use (registers.pc)

      system "shouldn't branch if the condition isn't met" 0x807AE1E $ do
        registers.pc .= 0x807AE1C
        registers.pc += 2
        flags.zero .= False
        interpretThumb $ ConditionalBranch EQ 30
        use (registers.pc)

    context "LongBranchWLink" $ do
      system "should be able to long branch properly" (0x0807AE02, 0x0807ACAF) $ do
        registers.pc .= 0x0807ACA8
        registers.pc += 2
        interpretThumb $ LongBranchWLink High 0
        registers.pc += 2
        interpretThumb $ LongBranchWLink Low 171
        (,) <$> use (registers.pc) <*> use (registers.lr)

    context "PushPopRegs" $ do
      system "should be able to push LR to the stack" (0x03007EFC, 0x0807ACBB) $ do
        registers.pc .= 0x0807ACA2
        registers.lr .= 0x0807ACBB
        registers.pc += 2
        interpretThumb $ PushPopRegs Store True []
        (,) <$> use (registers.sp) <*> readAddressWord 0x03007F00

system :: (Show a, Eq a) => String -> a -> SystemT Identity a -> Spec
system label val act = do
  romFile <- runIO $ ByteString.readFile "./res/suite.gba"
  it label $
    fst (runIdentity (runSystemT act (buildInitialState romFile ByteString.empty))) `shouldBe` val
