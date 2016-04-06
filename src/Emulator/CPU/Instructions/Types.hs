module Emulator.CPU.Instructions.Types where

import Emulator.CPU
import Emulator.Types

import Data.Int

data CPUMode = ARM | THUMB
  deriving (Show, Read, Eq, Ord)

newtype SetCondition = SetCondition Bool
  deriving (Show, Read, Eq, Ord)

data LoadStore = Load | Store
  deriving (Show, Read, Eq, Ord)

data PrePost = Pre | Post
  deriving (Show, Read, Eq, Ord)

data OffsetDirection = Up | Down
  deriving (Show, Read, Eq, Ord)

data AddSub = Add | Subtract
  deriving (Show, Read, Eq, Ord)

data LowHigh = Low | High
  deriving (Show, Read, Eq, Ord)

data Granularity = Byte | Word | HalfWord
  deriving (Show, Read, Eq, Ord)

newtype Immediate = Immediate Bool
  deriving (Show, Read, Eq, Ord)

data BaseSource = SP | PC
  deriving (Show, Read, Eq, Ord)

newtype Link = Link Bool
  deriving (Show, Read, Eq, Ord)

type WriteBack = Bool
type Signed = Bool
type Accumulate = Bool
type HighReg = Bool
type SignExtended = Bool
type StoreLR = Bool
type BranchOffset = Int32
type Offset = MWord
type Value = Int -- Value is used for signed values in instructions
type ForceUserMode = Bool
type RegisterList = [RegisterName]
