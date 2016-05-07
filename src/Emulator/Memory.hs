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
  , CanRead VRAM m, CanWrite VRAM m
  , CanRead IORegisters m, CanWrite IORegisters m
  , CanRead PaletteRAM m, CanWrite PaletteRAM m
  , CanRead GamePakWRAM m, CanWrite GamePakWRAM m )

writeAddressByte :: AddressSpace m => Address -> Byte -> m ()
writeAddressByte addr b = do
  case addressToRegionType addr of
    (_, BIOS) -> return ()
    (_, WRAM) -> writeByte (Proxy :: Proxy WRAM) addr b
    (_, GamePakWRAM) -> writeByte (Proxy :: Proxy GamePakWRAM) addr b
    (_, IORegisters) -> writeByte (Proxy :: Proxy IORegisters) addr b
    (_, PaletteRAM) -> writeByte (Proxy :: Proxy PaletteRAM) addr b
    (_, VRAM) -> writeByte (Proxy :: Proxy VRAM) addr b
    (_, ObjAttributes) -> writeByte (Proxy :: Proxy OAM) addr b
    (_, GamePakROM) -> return ()
    (_, GamePakSRAM) -> error "Undefined memory proxy location (GamePakSRAM)!"
    (_, Unused) -> return ()

readAddressByte :: AddressSpace m => Address -> m Byte
readAddressByte addr = do
  case addressToRegionType addr of
    (_, BIOS) -> readByte (Proxy :: Proxy BIOS) addr
    (_, WRAM) -> readByte (Proxy :: Proxy WRAM) addr
    (_, GamePakWRAM) -> readByte (Proxy :: Proxy GamePakWRAM) addr
    (_, IORegisters) -> readByte (Proxy :: Proxy IORegisters) addr
    (_, PaletteRAM) -> readByte (Proxy :: Proxy PaletteRAM) addr
    (_, VRAM) -> readByte (Proxy :: Proxy VRAM) addr
    (_, ObjAttributes) -> readByte (Proxy :: Proxy OAM) addr
    (_, GamePakROM) -> readByte (Proxy :: Proxy ROM) addr
    (_, GamePakSRAM) -> error "Undefined memory proxy location (GamePakSRAM)!"
    (_, Unused) -> return 0

writeAddressHalfWord :: AddressSpace m => Address -> HalfWord -> m ()
writeAddressHalfWord addr hw = do
  case addressToRegionType addr of
    (_, BIOS) -> return ()
    (_, WRAM) -> writeHalfWord (Proxy :: Proxy WRAM) addr hw
    (_, GamePakWRAM) -> writeHalfWord (Proxy :: Proxy GamePakWRAM) addr hw
    (_, IORegisters) -> writeHalfWord (Proxy :: Proxy IORegisters) addr hw
    (_, PaletteRAM) -> writeHalfWord (Proxy :: Proxy PaletteRAM) addr hw
    (_, VRAM) -> writeHalfWord (Proxy :: Proxy VRAM) addr hw
    (_, ObjAttributes) -> writeHalfWord (Proxy :: Proxy OAM) addr hw
    (_, GamePakROM) -> return ()
    (_, GamePakSRAM) -> error "Undefined memory proxy location (GamePakSRAM)!"
    (_, Unused) -> return ()

readAddressHalfWord :: AddressSpace m => Address -> m HalfWord
readAddressHalfWord addr = do
  case addressToRegionType addr of
    (_, BIOS) -> readHalfWord (Proxy :: Proxy BIOS) addr
    (_, WRAM) -> readHalfWord (Proxy :: Proxy WRAM) addr
    (_, GamePakWRAM) -> readHalfWord (Proxy :: Proxy GamePakWRAM) addr
    (_, IORegisters) -> readHalfWord (Proxy :: Proxy IORegisters) addr
    (_, PaletteRAM) -> readHalfWord (Proxy :: Proxy PaletteRAM) addr
    (_, VRAM) -> readHalfWord (Proxy :: Proxy VRAM) addr
    (_, ObjAttributes) -> readHalfWord (Proxy :: Proxy OAM) addr
    (_, GamePakROM) -> readHalfWord (Proxy :: Proxy ROM) addr
    (_, GamePakSRAM) -> error "Undefined memory proxy location (GamePakSRAM)!"
    (_, Unused) -> return 0

writeAddressWord :: AddressSpace m => Address -> MWord -> m ()
writeAddressWord addr w = do
  case addressToRegionType addr of
    (_, BIOS) -> return ()
    (_, WRAM) -> writeWord (Proxy :: Proxy WRAM) addr w
    (_, GamePakWRAM) -> writeWord (Proxy :: Proxy GamePakWRAM) addr w
    (_, IORegisters) -> writeWord (Proxy :: Proxy IORegisters) addr w
    (_, PaletteRAM) -> writeWord (Proxy :: Proxy PaletteRAM) addr w
    (_, VRAM) -> writeWord (Proxy :: Proxy VRAM) addr w
    (_, ObjAttributes) -> writeWord (Proxy :: Proxy OAM) addr w
    (_, GamePakROM) -> return ()
    (_, GamePakSRAM) -> error "Undefined memory proxy location (GamePakSRAM)!"
    (_, Unused) -> return ()

readAddressWord :: AddressSpace m => Address -> m MWord
readAddressWord addr = do
  case addressToRegionType addr of
    (_, BIOS) -> readWord (Proxy :: Proxy BIOS) addr
    (_, WRAM) -> readWord (Proxy :: Proxy WRAM) addr
    (_, GamePakWRAM) -> readWord (Proxy :: Proxy GamePakWRAM) addr
    (_, IORegisters) -> readWord (Proxy :: Proxy IORegisters) addr
    (_, PaletteRAM) -> readWord (Proxy :: Proxy PaletteRAM) addr
    (_, VRAM) -> readWord (Proxy :: Proxy VRAM) addr
    (_, ObjAttributes) -> readWord (Proxy :: Proxy OAM) addr
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
