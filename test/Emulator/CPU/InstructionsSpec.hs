module Emulator.CPU.InstructionsSpec where

import Emulator.CPU
import Emulator.CPU.Instructions

import Control.Lens
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.State
import Data.Default.Class
import Prelude hiding (Ordering(..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "runCondition" $ do

    prop "runCondition AL ~= True" $ \(x :: Flags) ->
      eval x (runCondition AL) == True

    it "should always run with an AL condition" $
      eval def (runCondition AL) `shouldBe` True

    context "when condition is EQ" $ do
      it "should run when zero is set" $ do
        eval (def & zero .~ True) (runCondition EQ) `shouldBe` True
      it "should not run when zero is not set" $ do
        eval (def & zero .~ False) (runCondition EQ) `shouldBe` False
      prop "runCondition EQ ~= use zero" $ \(x :: Flags) ->
        eval x (runCondition EQ) == eval x (use zero)

    context "when condition is NE" $ do
      it "should run when zero is clear" $ do
        eval (def & zero .~ False) (runCondition NE) `shouldBe` True
      it "should not run when zero is not clear" $ do
        eval (def & zero .~ True) (runCondition NE) `shouldBe` False
      prop "runCondition NE ~= not (use zero)" $ \(x :: Flags) ->
        eval x (runCondition NE) == eval x (not <$> use zero)
      prop "runCondition NE /= runCondition EQ" $ \(x :: Flags) ->
        eval x (runCondition NE) /= eval x (runCondition EQ)

    context "when condition is CS" $ do
      it "should run when carry is set" $ do
        eval (def & carry .~ True) (runCondition CS) `shouldBe` True
      it "should not run when carry is not set" $ do
        eval (def & carry .~ False) (runCondition CS) `shouldBe` False
      prop "runCondition CS ~= use carry" $ \(x :: Flags) ->
        eval x (runCondition CS) == eval x (use carry)

    context "when condition is CC" $ do
      it "should run when carry is clear" $ do
        eval (def & carry .~ False) (runCondition CC) `shouldBe` True
      it "should not run when carry is not clear" $ do
        eval (def & carry .~ True) (runCondition CC) `shouldBe` False
      prop "runCondition CC ~= not (use carry)" $ \(x :: Flags) ->
        eval x (runCondition CC) == eval x (not <$> use carry)
      prop "runCondition CC /= runCondition CS" $ \(x :: Flags) ->
        eval x (runCondition CC) /= eval x (runCondition CS)

    context "when condition is MI" $ do
      it "should run when negative is set" $ do
        eval (def & negative .~ True) (runCondition MI) `shouldBe` True
      it "should not run when negative is not set" $ do
        eval (def & negative .~ False) (runCondition MI) `shouldBe` False
      prop "runCondition MI ~= use negative" $ \(x :: Flags) ->
        eval x (runCondition MI) == eval x (use negative)

    context "when condition is PL" $ do
      it "should run when negative is clear" $ do
        eval (def & negative .~ False) (runCondition PL) `shouldBe` True
      it "should not run when negative is not clear" $ do
        eval (def & negative .~ True) (runCondition PL) `shouldBe` False
      prop "runCondition PL ~= not (use negative)" $ \(x :: Flags) ->
        eval x (runCondition PL) == eval x (not <$> use negative)
      prop "runCondition PL /= runCondition MI" $ \(x :: Flags) ->
        eval x (runCondition PL) /= eval x (runCondition MI)

    context "when condition is VS" $ do
      it "should run when overflow is set" $ do
        eval (def & overflow .~ True) (runCondition VS) `shouldBe` True
      it "should not run when overflow is not set" $ do
        eval (def & overflow .~ False) (runCondition VS) `shouldBe` False
      prop "runCondition VS ~= use overflow" $ \(x :: Flags) ->
        eval x (runCondition VS) == eval x (use overflow)

    context "when condition is VC" $ do
      it "should run when overflow is clear" $ do
        eval (def & overflow .~ False) (runCondition VC) `shouldBe` True
      it "should not run when overflow is not clear" $ do
        eval (def & overflow .~ True) (runCondition VC) `shouldBe` False
      prop "runCondition VC ~= not (use overflow)" $ \(x :: Flags) ->
        eval x (runCondition VC) == eval x (not <$> use overflow)
      prop "runCondition VC /= runCondition VS" $ \(x :: Flags) ->
        eval x (runCondition VC) /= eval x (runCondition VS)

instance Arbitrary Flags where
  arbitrary = mkFlags <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
  shrink = const []

newtype ConditionState a = ConditionState (State Flags a)
  deriving (Functor, Applicative, Monad, MonadState Flags)

eval :: Flags -> ConditionState a -> a
eval f (ConditionState x) = evalState x f

exec :: Flags -> ConditionState () -> Flags
exec f (ConditionState x) = execState x f
