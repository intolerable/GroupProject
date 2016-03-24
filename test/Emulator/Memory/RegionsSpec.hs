module Emulator.Memory.RegionsSpec where

import Emulator.Memory.Regions

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Region from address" $ do
    it "should correctly discretise memory addresses" $ do
      addressToRegionType 0x00000100 `shouldBe` (0x00000000, BIOS)
      addressToRegionType 0x00004001 `shouldBe` (0x00004000, Unused)
      addressToRegionType 0x06000000 `shouldBe` (0x06000000, VRAM)
      addressToRegionType 0x0E00FFFF `shouldBe` (0x0E000000, GamePakSRAM)
      addressToRegionType 0xFFFFFFFF `shouldBe` (0x0E010000, Unused)
