module Emulator.Types where

import qualified Data.Word

type Byte = Data.Word.Word8

type Halfword = Data.Word.Word16

type MWord = Data.Word.Word32

type Address = Data.Word.Word32

type RegisterVal = Address
