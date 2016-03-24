module Emulator.ROM.ParserSpec where

import Emulator.ROM
import Emulator.ROM.Parser

import Control.Lens
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "readROM" $ do
    it "should be able to read a ROM from a file" $ do
      readROM "./res/suite.gba" >>= \case
        Left err -> fail err
        Right (rh, _, _) -> do
          rh ^. magicByte `shouldBe` 0x96
