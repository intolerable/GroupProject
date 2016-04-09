module Emulator.Memory where

import Emulator.Memory.AddressSpace
import Emulator.Memory.Regions
import Emulator.Types

import Data.Proxy

type AddressSpace m =
  ( Functor m, Monad m
  , CanRead WRAM m, CanWrite WRAM m
  , CanRead ROM m
  , CanRead OAM m, CanWrite OAM m
  , CanRead VRAM m, CanWrite VRAM m )

writeAddressHalfWord :: AddressSpace m => Address -> HalfWord -> m ()
writeAddressHalfWord addr hw =
  case addressToRegionType addr of
    (_, BIOS) -> return ()
    (_, WRAM) -> writeHalfWord (Proxy :: Proxy WRAM) addr hw
    (_, GamePakWRAM) -> writeHalfWord (Proxy :: Proxy WRAM) addr hw
    (_, IORegisters) -> writeHalfWord (Proxy :: Proxy WRAM) addr hw
    (_, PaletteRAM) -> writeHalfWord (Proxy :: Proxy WRAM) addr hw
    (_, VRAM) -> writeHalfWord (Proxy :: Proxy VRAM) addr hw
    (_, ObjAttributes) -> writeHalfWord (Proxy :: Proxy OAM) addr hw
    (_, GamePakROM) -> return ()
    (_, GamePakSRAM) -> writeHalfWord (Proxy :: Proxy OAM) addr hw
    (_, Unused) -> return ()

readAddressHalfWord :: AddressSpace m => Address -> m HalfWord
readAddressHalfWord addr =
  case addressToRegionType addr of
    (_, BIOS) -> return 0
    (_, WRAM) -> readHalfWord (Proxy :: Proxy WRAM) addr
    (_, GamePakWRAM) -> readHalfWord (Proxy :: Proxy WRAM) addr
    (_, IORegisters) -> readHalfWord (Proxy :: Proxy WRAM) addr
    (_, PaletteRAM) -> readHalfWord (Proxy :: Proxy WRAM) addr
    (_, VRAM) -> readHalfWord (Proxy :: Proxy VRAM) addr
    (_, ObjAttributes) -> readHalfWord (Proxy :: Proxy OAM) addr
    (_, GamePakROM) -> readHalfWord (Proxy :: Proxy OAM) addr
    (_, GamePakSRAM) -> readHalfWord (Proxy :: Proxy OAM) addr
    (_, Unused) -> return 0

writeAddressWord :: AddressSpace m => Address -> MWord -> m ()
writeAddressWord addr hw =
  case addressToRegionType addr of
    (_, BIOS) -> return ()
    (_, WRAM) -> writeWord (Proxy :: Proxy WRAM) addr hw
    (_, GamePakWRAM) -> writeWord (Proxy :: Proxy WRAM) addr hw
    (_, IORegisters) -> writeWord (Proxy :: Proxy WRAM) addr hw
    (_, PaletteRAM) -> writeWord (Proxy :: Proxy WRAM) addr hw
    (_, VRAM) -> writeWord (Proxy :: Proxy WRAM) addr hw
    (_, ObjAttributes) -> writeWord (Proxy :: Proxy WRAM) addr hw
    (_, GamePakROM) -> return ()
    (_, GamePakSRAM) -> writeWord (Proxy :: Proxy WRAM) addr hw
    (_, Unused) -> return ()

readAddressWord :: AddressSpace m => Address -> m MWord
readAddressWord addr =
  case addressToRegionType addr of
    (_, BIOS) -> return 0
    (_, WRAM) -> readWord (Proxy :: Proxy WRAM) addr
    (_, GamePakWRAM) -> readWord (Proxy :: Proxy WRAM) addr
    (_, IORegisters) -> readWord (Proxy :: Proxy WRAM) addr
    (_, PaletteRAM) -> readWord (Proxy :: Proxy WRAM) addr
    (_, VRAM) -> readWord (Proxy :: Proxy WRAM) addr
    (_, ObjAttributes) -> readWord (Proxy :: Proxy WRAM) addr
    (_, GamePakROM) -> readWord (Proxy :: Proxy WRAM) addr
    (_, GamePakSRAM) -> readWord (Proxy :: Proxy WRAM) addr
    (_, Unused) -> return 0

