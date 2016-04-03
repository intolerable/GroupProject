module Emulator.Memory.RegionsSpec where

import Emulator.Memory.Regions

import Data.Maybe
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "addressToRegionType" $ do
    it "should be able to correctly discretise memory addresses" $ do
      addressToRegionType 0x00000100 `shouldBe` (0x00000000, BIOS)
      addressToRegionType 0x00004001 `shouldBe` (0x00004000, Unused)
      addressToRegionType 0x06000000 `shouldBe` (0x06000000, VRAM)
      addressToRegionType 0x0E00FFFF `shouldBe` (0x0E000000, GamePakSRAM)
      addressToRegionType 0xFFFFFFFF `shouldBe` (0x0E010000, Unused)

  describe "lookupRegion" $ do

    describe "for any address we generate, we should have a defined use for that address" $
      prop "isJust (lookupRegion addr) == True" $ \addr ->
        isJust $ lookupRegion addr

    it "should have defined uses for edge-case addresses" $ do
      lookupRegion 0x00000000 `shouldSatisfy` isJust
      lookupRegion 0x00000001 `shouldSatisfy` isJust
      lookupRegion 0xFFFFFFFF `shouldSatisfy` isJust
