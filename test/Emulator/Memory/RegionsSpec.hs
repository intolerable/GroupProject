module Emulator.Memory.RegionsSpec where


import Emulator.Memory.Regions

import Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Region from address" $ do
    it "should correctly discretise memory addresses" $ do
      addressToRegionType 0x00000100 `shouldBe` BIOS
      addressToRegionType 0x00004001 `shouldBe` Unused
      addressToRegionType 0x06000000 `shouldBe` VRAM
      addressToRegionType 0x0E00FFFF `shouldBe` GamePakSRAM
      addressToRegionType 0xFFFFFFFF `shouldBe` Unused
