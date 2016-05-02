module Emulator.Memory where

import Emulator.Memory.AddressSpace
import Emulator.Memory.Regions
import Emulator.Types
import Utilities.Parser.TemplateHaskell

import Data.Array
import Data.Bits
import Data.Proxy

type AddressSpace m =
  ( Functor m, Monad m
  , CanRead WRAM m, CanWrite WRAM m
  , CanRead ROM m
  , CanRead BIOS m
  , CanRead OAM m, CanWrite OAM m
  , CanRead VRAM m, CanWrite VRAM m )

writeAddressByte :: AddressSpace m => Address -> Byte -> m ()
writeAddressByte addr hw =
  case addressToRegionType addr of
    (_, BIOS) -> return ()
    (_, WRAM) -> writeByte (Proxy :: Proxy WRAM) addr hw
    (_, GamePakWRAM) -> error "Undefined memory proxy location (GamePakWRAM)!"
    (_, IORegisters) -> error "Undefined memory proxy location (IORegisters)!"
    (_, PaletteRAM) -> error "Undefined memory proxy location (PaletteRAM)!"
    (_, VRAM) -> writeByte (Proxy :: Proxy VRAM) addr hw
    (_, ObjAttributes) -> writeByte (Proxy :: Proxy OAM) addr hw
    (_, GamePakROM) -> return ()
    (_, GamePakSRAM) -> error "Undefined memory proxy location (GamePakSRAM)!"
    (_, Unused) -> return ()

readAddressByte :: AddressSpace m => Address -> m Byte
readAddressByte addr =
  case addressToRegionType addr of
    (_, BIOS) -> readByte (Proxy :: Proxy BIOS) addr
    (_, WRAM) -> readByte (Proxy :: Proxy WRAM) addr
    (_, GamePakWRAM) -> error "Undefined memory proxy location (GamePakWRAM)!"
    (_, IORegisters) -> error "Undefined memory proxy location (IORegisters)!"
    (_, PaletteRAM) -> error "Undefined memory proxy location (PaletteRAM)!"
    (_, VRAM) -> readByte (Proxy :: Proxy VRAM) addr
    (_, ObjAttributes) -> readByte (Proxy :: Proxy OAM) addr
    (_, GamePakROM) -> readByte (Proxy :: Proxy ROM) addr
    (_, GamePakSRAM) -> error "Undefined memory proxy location (GamePakSRAM)!"
    (_, Unused) -> return 0

writeAddressHalfWord :: AddressSpace m => Address -> HalfWord -> m ()
writeAddressHalfWord addr hw =
  case addressToRegionType addr of
    (_, BIOS) -> return ()
    (_, WRAM) -> writeHalfWord (Proxy :: Proxy WRAM) addr hw
    (_, GamePakWRAM) -> error "Undefined memory proxy location (GamePakWRAM)!"
    (_, IORegisters) -> error "Undefined memory proxy location (IORegisters)!"
    (_, PaletteRAM) -> error "Undefined memory proxy location (PaletteRAM)!"
    (_, VRAM) -> writeHalfWord (Proxy :: Proxy VRAM) addr hw
    (_, ObjAttributes) -> writeHalfWord (Proxy :: Proxy OAM) addr hw
    (_, GamePakROM) -> return ()
    (_, GamePakSRAM) -> error "Undefined memory proxy location (GamePakSRAM)!"
    (_, Unused) -> return ()

readAddressHalfWord :: AddressSpace m => Address -> m HalfWord
readAddressHalfWord addr =
  case addressToRegionType addr of
    (_, BIOS) -> readHalfWord (Proxy :: Proxy BIOS) addr
    (_, WRAM) -> readHalfWord (Proxy :: Proxy WRAM) addr
    (_, GamePakWRAM) -> error "Undefined memory proxy location (GamePakWRAM)!"
    (_, IORegisters) -> error "Undefined memory proxy location (IORegisters)!"
    (_, PaletteRAM) -> error "Undefined memory proxy location (PaletteRAM)!"
    (_, VRAM) -> readHalfWord (Proxy :: Proxy VRAM) addr
    (_, ObjAttributes) -> readHalfWord (Proxy :: Proxy OAM) addr
    (_, GamePakROM) -> readHalfWord (Proxy :: Proxy ROM) addr
    (_, GamePakSRAM) -> error "Undefined memory proxy location (GamePakSRAM)!"
    (_, Unused) -> return 0

writeAddressWord :: AddressSpace m => Address -> MWord -> m ()
writeAddressWord addr hw =
  case addressToRegionType addr of
    (_, BIOS) -> return ()
    (_, WRAM) -> writeWord (Proxy :: Proxy WRAM) addr hw
    (_, GamePakWRAM) -> error "Undefined memory proxy location (GamePakWRAM)!"
    (_, IORegisters) -> error "Undefined memory proxy location (IORegisters)!"
    (_, PaletteRAM) -> error "Undefined memory proxy location (PaletteRAM)!"
    (_, VRAM) -> writeWord (Proxy :: Proxy WRAM) addr hw
    (_, ObjAttributes) -> error "Undefined memory proxy location (ObjAttributes)!"
    (_, GamePakROM) -> return ()
    (_, GamePakSRAM) -> error "Undefined memory proxy location (GamePakSRAM)!"
    (_, Unused) -> return ()

readAddressWord :: AddressSpace m => Address -> m MWord
readAddressWord addr =
  case addressToRegionType addr of
    (_, BIOS) -> readWord (Proxy :: Proxy BIOS) addr
    (_, WRAM) -> readWord (Proxy :: Proxy WRAM) addr
    (_, GamePakWRAM) -> error "Undefined memory proxy location (GamePakWRAM)!"
    (_, IORegisters) -> error "Undefined memory proxy location (IORegisters)!"
    (_, PaletteRAM) -> error "Undefined memory proxy location (PaletteRAM)!"
    (_, VRAM) -> error "Undefined memory proxy location (VRAM)!"
    (_, ObjAttributes) -> error "Undefined memory proxy location (ObjAttributes)!"
    (_, GamePakROM) -> readWord (Proxy :: Proxy ROM) addr
    (_, GamePakSRAM) -> error "Undefined memory proxy location (GamePakSRAM)!"
    (_, Unused) -> return 0

readRange :: AddressSpace m => (Address, Address) -> m (Array Address Byte)
readRange r = listArray r <$> mapM readAddressByte (range r)

readWords :: AddressSpace m => Address -> Int -> m [MWord]
readWords _ 0 = return []
readWords a n = (:) <$> readAddressWord a <*> readWords (a + 4) (n - 1)

readBytes :: AddressSpace m => Address -> Int -> m [Byte]
readBytes _ 0 = return []
readBytes a 1 = do
  hw <- readAddressHalfWord a
  return [readLowerByte hw]
readBytes a 2 = do
  hw <- readAddressHalfWord a
  return [readLowerByte hw, readUpperByte hw]
readBytes a 3 = (++) <$> readBytes a 2 <*> readBytes (a+2) 1
readBytes a n = do
  w <- readAddressWord a
  let b = splitWord w
  ws <- readBytes (a+4) (n-4)
  return $ b ++ ws
  where
    splitWord :: MWord -> [Byte]
    splitWord w =
      [fromIntegral $ ($(bitmask 31 24) w) `shiftR` 24,
      fromIntegral $ ($(bitmask 22 16) w) `shiftR` 16,
      fromIntegral $ ($(bitmask 15 8) w) `shiftR` 8,
      fromIntegral $ $(bitmask 7 0) w]

readUpperByte :: HalfWord -> Byte
readUpperByte hw = fromIntegral $ ($(bitmask 15 8) hw) `shiftR` 8

readLowerByte :: HalfWord -> Byte
readLowerByte hw = fromIntegral $ $(bitmask 7 0) hw
