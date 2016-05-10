module Emulator.CPU.Instructions.ARM.OpcodesSpec where

import Emulator.CPU
import Emulator.CPU.Instructions.ARM.Opcodes
import Emulator.Types

import Control.Lens
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.State
import Data.Default.Class
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "cmp" $ do

    context "CMP _ (r1 = 5) 0" $ do
      let res = exec (def & r1 .~ 0x5) $ cmp () r1 (operand2Lens $ Right $ Rotated 0 0) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x0
      it "should set carry" $
        res ^. flags.carry `shouldBe` True
      it "should not set overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should not set zero" $
        res ^. flags.zero `shouldBe` False
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "CMP _ (r1 = 0) 0" $ do
      let res = exec (def & r1 .~ 0x0) $ cmp () r1 (operand2Lens $ Right $ Rotated 0 0) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x0
      it "should set carry" $
        res ^. flags.carry `shouldBe` True
      it "should not set overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should not set zero" $
        res ^. flags.zero `shouldBe` True
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "CMP _ (r1 = a) a" $ do
      let res = exec (def & r1 .~ 0xA) $ cmp () r1 (operand2Lens $ Right $ Rotated 0 0xA) True
      it "should set the zero flag, false the negative, carry and overflow" $ do
        res ^. flags.carry `shouldBe` False
        res ^. flags.zero `shouldBe` True
        res ^. flags.overflow `shouldBe` False
        res ^. flags.negative `shouldBe` False

  describe "mov" $ do

    context "MOV r0 _ 5" $ do
      let res = exec def $ mov r0 () (operand2Lens $ Right $ Rotated 0 5) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x5
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should not set zero" $
        res ^. flags.zero `shouldBe` False
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "MOV r0 _ 0" $ do
      let res = exec def $ mov r0 () (operand2Lens $ Right $ Rotated 0 0) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x0
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should not set zero" $
        res ^. flags.zero `shouldBe` True
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "MOV r0 _ (r2 = 1 << 2)" $ do
      let res = exec (def & r2 .~ 0x1) $ mov r0 () (operand2Lens $ Left $ AmountShift 2 LogicalLeft $ RegisterName 2) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x4
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should not set zero" $
        res ^. flags.zero `shouldBe` False
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "MOV r0 _ (r2 = 15 << r3 = 2)" $ do
      let res = exec (def & r2 .~ 0xF & r3 .~ 0x2) $ mov r0 () (operand2Lens $ Left $ RegisterShift (RegisterName 3) LogicalLeft (RegisterName 2)) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x3C
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should not set zero" $
        res ^. flags.zero `shouldBe` False
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "MOV r0 _ (r15 = 0x00000000)" $ return ()

  describe "add" $ do

    context "ADD r0 (r1 = 0) 0" $ do
      let res = exec def $ add r0 r1 (operand2Lens $ Right $ Rotated 0 0) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x0
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should set zero" $
        res ^. flags.zero `shouldBe` True
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "ADD r0 (r1 = 0xFFFFFFFF) (r2 = 0xFFFFFFFF)" $ do
      let res = exec (def & r1 .~ 0xFFFFFFFF & r2 .~ 0xFFFFFFFF) $ add r0 r1 (operand2Lens $ Left $ AmountShift 0 LogicalLeft $ RegisterName 2) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0xFFFFFFFE
      it "should set carry" $
        res ^. flags.carry `shouldBe` True
      it "should set overflow" $
        res ^. flags.overflow `shouldBe` True
      it "should not set zero" $
        res ^. flags.zero `shouldBe` False
      it "should set negative" $
        res ^. flags.negative `shouldBe` True

  describe "cmn" $ do

    context "CMN _ (r1 = 0) 0" $ do
      let res = exec def $ cmn () r1 (operand2Lens $ Right $ Rotated 0 0) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x0
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should set zero" $
        res ^. flags.zero `shouldBe` True
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "CMN _ (r1 = 0) 1" $ do
      let res = exec def $ cmn () r1 (operand2Lens $ Right $ Rotated 0 1) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x0
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should not set zero" $
        res ^. flags.zero `shouldBe` False
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "CMN _ (r1 = 0xFFFFFFFF) (r2 = 0xFFFFFFFF)" $ do
      let res = exec (def & r1 .~ 0xFFFFFFFF & r2 .~ 0xFFFFFFFF) $ cmn () r1 (operand2Lens $ Left $ AmountShift 0 LogicalLeft $ RegisterName 2) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x0
      it "should set carry" $
        res ^. flags.carry `shouldBe` True
      it "should set overflow" $
        res ^. flags.overflow `shouldBe` True
      it "should not set zero" $
        res ^. flags.zero `shouldBe` False
      it "should set negative" $
        res ^. flags.negative `shouldBe` True

  describe "teq" $ do

    context "TEQ _ (r1 = 0) 0" $ do
      let res = exec (def & r0 .~ 0x1) $ teq () r1 (operand2Lens $ Right $ Rotated 0 0) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x1
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should set zero" $
        res ^. flags.zero `shouldBe` True
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "TEQ _ (r1 = 2) 1" $ do
      let res = exec (def & r1 .~ 0x2 & flags.zero .~ True) $ teq () r1 (operand2Lens $ Right $ Rotated 0 1) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x0
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should not set zero" $
        res ^. flags.zero `shouldBe` False
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

  describe "orr" $ do

    context "ORR r0 (r1 = 0) 0" $ do
      let res = exec def $ orr r0 r1 (operand2Lens $ Right $ Rotated 0 0) True
      it "should set result" $
        res ^. r0 `shouldBe` 0x0
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should set zero" $
        res ^. flags.zero `shouldBe` True
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "ORR r0 (r1 = 1) 4" $ do
      let res = exec (def & r1 .~ 0x1) $ orr r0 r1 (operand2Lens $ Right $ Rotated 0 4) True
      it "should set result" $
        res ^. r0 `shouldBe` 0x5
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should not set zero" $
        res ^. flags.zero `shouldBe` False
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

  describe "tst" $ do

    context "TST _ (r1 = 0) 0" $ do
      let res = exec def $ tst () r1 (operand2Lens $ Right $ Rotated 0 0) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x0
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should set zero" $
        res ^. flags.zero `shouldBe` True
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

    context "TST _ (r1 = 3) 2" $ do
      let res = exec (def & r1 .~ 0x3) $ tst () r1 (operand2Lens $ Right $ Rotated 0 2) True
      it "should not set result" $
        res ^. r0 `shouldBe` 0x0
      it "should not affect carry" $
        res ^. flags.carry `shouldBe` False
      it "should not affect overflow" $
        res ^. flags.overflow `shouldBe` False
      it "should set zero" $
        res ^. flags.zero `shouldBe` False
      it "should not set negative" $
        res ^. flags.negative `shouldBe` False

newtype OpcodeState a = OpcodeState (State Registers a)
  deriving (Functor, Applicative, Monad, MonadState Registers)

exec :: Registers -> OpcodeState () -> Registers
exec r (OpcodeState x) = execState x r
