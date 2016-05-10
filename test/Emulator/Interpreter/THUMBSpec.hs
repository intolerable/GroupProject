module Emulator.Interpreter.THUMBSpec where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.Interpreter.Monad
import Emulator.Interpreter.THUMB
import Emulator.Memory
import Emulator.Types

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

      system "should be able to pop PC from the stack" (0x00C0FFEE, 0x03007F00) $ do
        registers.pc .= 0x0807ACA2
        currentSP <- use (registers.sp)
        writeAddressWord currentSP 0x00C0FFEE
        registers.sp -= 4
        interpretThumb $ PushPopRegs Load True []
        (,) <$> use (registers.pc) <*> use (registers.sp)

      system "should be able to pop multiple values from the stack" (0x00C0FFEE, 0x00BADA55, 0x0D15EA5E, 0x0807ACA2) $ do
        registers.pc .= 0x0807ACA2
        (registers.sp <<-= 4) >>= \x -> writeAddressWord x 0x00C0FFEE
        (registers.sp <<-= 4) >>= \x -> writeAddressWord x 0x00BADA55
        (registers.sp <<-= 4) >>= \x -> writeAddressWord x 0x0D15EA5E
        interpretThumb $ PushPopRegs Load False [RegisterName 0, RegisterName 1, RegisterName 2]
        (,,,) <$> use (registers.r0) <*> use (registers.r1) <*> use (registers.r2) <*> use (registers.pc)

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

    context "MovAddCmpSubImmediate" $ do

      system "should be able to MOV a value into a register" 0xF $ do
        registers.pc .= 0x0807ACA4
        registers.pc += 2
        interpretThumb $ MovCmpAddSubImmediate MOV (RegisterName 0) 0xF
        use (registers.r0)

    context "LoadStoreHalfword" $ do

      system "should be able to load a halfword from memory" 0x2411 $ do
        registers.pc .= 0x08000000
        registers.r6 .= 0x0800000A
        interpretThumb $ ThumbLoadStoreHalfword Load 10 (RegisterName 6) (RegisterName 2)
        use $ registers.r2

    context "ThumbLoadStoreImmediateOffset" $ do

      system "should be able to load a word from a register address" 0xE3A00301 $ do
        registers.pc .= 0x0807AE48
        registers.pc += 2
        registers.r2 .= 0x080000E0
        interpretThumb $
          ThumbLoadStoreImmediateOffset Word Load 0 (RegisterName 2) (RegisterName 1)
        use (registers.r1)

    context "MultipleLoadStore" $ do

      system "should be able to write a register to an address" 0x00C0FFEE $ do
        registers.pc .= 0x08000192
        registers.r0 .= 0x00C0FFEE
        registers.r2 .= 0x02000000
        registers.pc += 2
        interpretThumb $ MultipleLoadStore Store (RegisterName 2) [RegisterName 0]
        readAddressWord 0x02000000

      system "should be able to multiple registers to an address" (0x00C0FFEE, 0xDEADBEEF) $ do
        registers.pc .= 0x08000192
        registers.r0 .= 0x00C0FFEE
        registers.r1 .= 0xDEADBEEF
        registers.r2 .= 0x02000000
        registers.pc += 2
        interpretThumb $ MultipleLoadStore Store (RegisterName 2) [RegisterName 0, RegisterName 1]
        (,) <$> readAddressWord 0x02000000 <*> readAddressWord 0x02000004

      system "should be able to read a register from an address" 0x00C0FFEE $ do
        registers.pc .= 0x08000192
        writeAddressWord 0x02000000 0x00C0FFEE
        registers.r2 .= 0x02000000
        registers.pc += 2
        interpretThumb $ MultipleLoadStore Load (RegisterName 2) [RegisterName 0]
        use (registers.r0)

    context "MovCmpAddSubImmediate" $ do
      -- (n, z, c, v)
      system "Should be able to compare some things" (True, False, False, False) $ do
        registers.r0 .= 0x0
        interpretThumb $ MovCmpAddSubImmediate CMP (RegisterName 0) 1
        (,,,) <$> use (flags.negative) <*> use (flags.zero) <*> use (flags.carry) <*> use (flags.overflow)

      system "Should be able to set the zero and carry happily" (False, True, True, False) $ do
        registers.r0 .= 0x0
        interpretThumb $ MovCmpAddSubImmediate CMP (RegisterName 0) 0
        (,,,) <$> use (flags.negative) <*> use (flags.zero) <*> use (flags.carry) <*> use (flags.overflow)

system :: (Show a, Eq a) => String -> a -> SystemT Identity a -> Spec
system label val act = do
  romFile <- runIO $ ByteString.readFile "./res/suite.gba"
  it label $
    fst (runIdentity (runSystemT act (buildInitialState romFile ByteString.empty))) `shouldBe` val
