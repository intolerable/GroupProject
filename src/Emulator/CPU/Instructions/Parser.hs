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

parseARM :: MWord -> Either String (Condition, Instruction ARM)
parseARM w 
  | w .&. 0x0FFFFFF0 == 0b00000001001011111111111100010000 = undefined -- Definitely branch exchange instruction
  | (w .&. 0x0C000000 == 0x00) && (testBit w 25 || w .&. 0b11110000 /= 0b10010000) = undefined -- Data Processing thing
    --Right (getCondition w, DataProcessing _ _ _ _ _)
  | w .&. 0x0FB00FF0 == 0x01000090 = undefined -- Single data swap
  | otherwise = 
    case w .&. 0x0E000000 of -- Test the identity bits
      0x00 -> if (w .&. 0x010000F0) == 0x90 then undefined -- multiply 
              else undefined -- halfword data transfer
      0x08000000 -> undefined -- Block data transfer
      0x0A000000 -> undefined -- Branch instruction
      0x0C000000 -> undefined -- Coprocessor data transfer
      0x0E000000 -> undefined -- Coprocessor data operation
      x | x == 0x6000000 || x == 0x4000000 -> undefined -- Load/Store
      _ -> undefined -- BAD OPCODE!!!

parseTHUMB :: HalfWord -> Either String (Instruction THUMB)
parseTHUMB = undefined

getCondition :: MWord -> Condition
getCondition w = 
  case conditionFromByte $ fromIntegral $ (w .&. 0xF0000000) `shiftR` 28 of
    Just x -> x
    Nothing -> error "undefined condition!"