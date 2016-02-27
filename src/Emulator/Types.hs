module Emulator.Types where

import qualified Data.Word

type Byte = Data.Word.Word8

type HalfWord = Data.Word.Word16

type MWord = Data.Word.Word32

type Address = Data.Word.Word32

type DWord = Data.Word.Word64

type RegisterVal = Address
