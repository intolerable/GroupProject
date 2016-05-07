module Emulator.Interpreter.THUMBSpec where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.Interpreter.Monad
import Emulator.Interpreter.THUMB

import Control.Lens
import Test.Hspec
import Prelude hiding (Ordering(..))
import qualified Data.ByteString.Lazy as ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "interpretThumb" $ do

    romFile <- runIO $ ByteString.readFile "./res/suite.gba"
    let test = trySystem (buildInitialState romFile ByteString.empty)

    context "ConditionalBranch" $ do
      test "can manage a basic conditional branch" 0x0807AE68 $ do
        registers.pc .= 0x0807AE52
        flags.zero .= True
        interpretThumb $ ConditionalBranch EQ 18
        registers.pc += 2
        use (registers.pc)

trySystem :: (Show a, Eq a) => SystemState -> String -> a -> SystemT Identity a -> SpecWith ()
trySystem start label val act = do
  it label $
    fst (runIdentity (runSystemT act start)) `shouldBe` val
