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

newtype SetCondition = SetCondition Bool
  deriving (Show, Read, Eq)

data LoadStore = Load | Store
  deriving (Show, Read, Eq)

data PrePost = Pre | Post
  deriving (Show, Read, Eq)

data UpDown = Up | Down
  deriving (Show, Read, Eq)

type WriteBack = Bool
type Rotated a = a
type Signed = Bool
type Granularity = ()
type Accumulate = Bool
type Link = Bool
type Offset = MWord
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
  | w .&. 0x0FFFFFF0 == 0b00000001001011111111111100010000 = Right (getCondition w, readBranchExchange w) -- Definitely branch exchange instruction
  | (w .&. 0x0C000000 == 0x00) && (testBit w 25 || w .&. 0b11110000 /= 0b10010000) = -- Data Processing thing
    Right (getCondition w,
      DataProcessing (getOpcode w)
                     (SetCondition $ w `testBit` 20)
                     (RegisterName $ fromIntegral $ (w .&. 0x000F0000) `shiftR` 15)
                     (RegisterName $ fromIntegral $ (w .&. 0x0000F000) `shiftR` 11)
                     (parseShiftedRegister (w `testBit` 25) w))
  | w .&. 0x0FB00FF0 == 0x01000090 = undefined -- Single data swap
  | otherwise =
    case w .&. 0x0E000000 of -- Test the identity bits
      0x00 -> if (w .&. 0x010000F0) == 0x90 then undefined -- multiply
              else undefined -- halfword data transfer
      0x08000000 -> undefined -- Block data transfer
      0x0A000000 -> Right (getCondition w, readBranch w) -- Branch instruction
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

getOpcode :: MWord -> Opcode
getOpcode w =
  case opcodeFromByte $ fromIntegral $ (w .&. 0x01E00000) `shiftR` 20 of
    Just x -> x
    Nothing -> error "undefined opcode!"

parseShiftedRegister :: Bool -> MWord -> Either (Shifted RegisterName) (Rotated Byte)
parseShiftedRegister True = undefined
parseShiftedRegister False = undefined

readBranchExchange :: MWord -> Instruction ARM
readBranchExchange br = BranchExchange $ RegisterName $ fromIntegral val
  where
    val = br .&. 0b1111

readBranch :: MWord -> Instruction ARM
readBranch br = Branch linkBit offset
  where
    linkBit = testBit br 24
    offset = br .&. 0xFFFFFF
