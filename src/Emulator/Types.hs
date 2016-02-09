module Emulator.Types where

import qualified Data.Word

type Byte = Data.Word.Word8

type Word = Data.Word.Word16

type Address = Data.Word.Word32

type RegisterVal = Address
