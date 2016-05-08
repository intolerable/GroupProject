module Emulator.Interpreter.THUMBSpec where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.Types
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

    context "PCRelativeLoad" $ do
      system "should be able to load an address to a register" 0x080237C4 $ do
        registers.pc .= 0x080003A0
        registers.pc += 2
        interpretThumb $ PCRelativeLoad (RegisterName 0) 96
        -- Should load address in 0x08000400, which in suite is 0x080237C4
        use $ registers.r0

    context "HiRegOperation" $ do

      system "should be able to execute a MOV from a lower register to an upper register" 0x00C0FFEE $ do
        registers.pc .= 0x0801277C
        registers.r4 .= 0x00C0FFEE
        registers.pc += 2
        interpretThumb $ HiRegOperation T_MOV (RegisterName 4) (RegisterName 10)
        use (registers.r10)

      system "should be able to execute a MOV from an upper register to a lower register" 0x00C0FFEE $ do
        registers.pc .= 0x0801277C
        registers.r10 .= 0x00C0FFEE
        registers.pc += 2
        interpretThumb $ HiRegOperation T_MOV (RegisterName 10) (RegisterName 4)
        use (registers.r4)


system :: (Show a, Eq a) => String -> a -> SystemT Identity a -> Spec
system label val act = do
  romFile <- runIO $ ByteString.readFile "./res/suite.gba"
  it label $
    fst (runIdentity (runSystemT act (buildInitialState romFile ByteString.empty))) `shouldBe` val
