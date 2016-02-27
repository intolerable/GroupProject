module Emulator.CPU.Instructions.Parser where

import Emulator.CPU
import Emulator.Types

import Data.Bits

data CPUMode = ARM | THUMB
  deriving (Show, Read, Eq)

type ARM = 'ARM
type THUMB = 'THUMB

newtype RegisterName = RegisterName Int
  deriving (Show, Read, Eq, Ord)

data LoadStore = Load | Store
  deriving (Show, Read, Eq)

data PrePost = Pre | Post
  deriving (Show, Read, Eq)

data UpDown = Up | Down
  deriving (Show, Read, Eq)

type WriteBack = Bool
type SetCondition = ()
type Rotated a = a
type Signed = ()
type Granularity = ()
type Accumulate = ()
type Link = ()
type Offset = ()
type ForceUserMode = ()
type RegisterList = ()

data Instruction a where
  DataProcessing :: Opcode -> SetCondition -> RegisterName -> RegisterName -> Either (Shifted RegisterName) (Rotated Byte) -> Instruction ARM
  Multiply :: Accumulate -> SetCondition -> RegisterName -> RegisterName -> RegisterName -> RegisterName -> Instruction ARM
    -- MUL A S Rd Rn Rs Rm, Rd <- Rm * Rs (+ Rn if A)
  MultiplyLong :: Signed -> Accumulate -> SetCondition -> RegisterName -> RegisterName -> RegisterName -> RegisterName -> Instruction ARM
  SingleDataSwap :: Granularity -> RegisterName -> RegisterName -> RegisterName -> Instruction ARM
  BranchExchange :: RegisterName -> Instruction ARM
  HalfwordDataTransferRegister :: PrePost -> UpDown -> WriteBack -> LoadStore -> Signed -> Granularity -> RegisterName -> RegisterName -> RegisterName -> Instruction ARM
  HalfwordDataTransferImmediate :: PrePost -> UpDown -> WriteBack -> LoadStore -> Signed -> Granularity -> RegisterName -> RegisterName -> Offset -> Instruction ARM
  SingleDataTransfer :: PrePost -> UpDown -> Granularity -> WriteBack -> LoadStore -> RegisterName -> RegisterName -> Either (Shifted RegisterName) Offset -> Instruction ARM
  Undefined :: Instruction ARM
  BlockDataTransfer :: PrePost -> UpDown -> ForceUserMode -> WriteBack -> LoadStore -> RegisterName -> RegisterList -> Instruction ARM
  Branch :: Link -> Offset -> Instruction ARM
  CoprocessorDataTransfer :: Instruction ARM
  CoprocessorDataOperation :: Instruction ARM
  CoprocessorRegisterTransfer :: Instruction ARM
  SoftwareInterrupt :: Instruction ARM

getCondition :: MWord -> Condition
getCondition w = 
  case conditionFromByte $ fromIntegral $ (w .&. 0xF0000000) `shiftR` 28 of
    Just x -> x
    Nothing -> error "undefined condition!"