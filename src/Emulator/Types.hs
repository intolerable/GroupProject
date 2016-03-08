module Emulator.Types where

import Data.Array.IO
import Data.Array.Unboxed
import qualified Data.Word

type Byte = Data.Word.Word8

type HalfWord = Data.Word.Word16

type MWord = Data.Word.Word32

type Address = Data.Word.Word32

type DWord = Data.Word.Word64

type RegisterVal = Address

type Memory = UArray Address Byte

type MemoryIO = IOUArray Address Byte
