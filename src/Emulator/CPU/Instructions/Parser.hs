module Emulator.CPU.Instructions.Parser where

import Emulator.CPU
import Emulator.Types

import Data.Bits
import Data.Maybe

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

data Granularity = Byte | Word | HalfWord
  deriving (Show, Read, Eq)

newtype Immediate = Immediate Bool
  deriving (Show, Read, Eq)

data Rotated a = Rotated Int a
  deriving (Show, Read, Eq)

type WriteBack = Bool
type Signed = Bool
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

deriving instance Show (Instruction a)

parseARM :: MWord -> Either String (Condition, Instruction ARM)
parseARM w
  | w .&. 0x0FFFFFF0 == 0b00000001001011111111111100010000 = Right (getCondition w, readBranchExchange w) -- Definitely branch exchange instruction
  | (w .&. 0x0C000000 == 0x00) && (testBit w 25 || w .&. 0b11110000 /= 0b10010000) = -- Data Processing thing
    Right (getCondition w,
      DataProcessing (getOpcode w)
                     (SetCondition $ w `testBit` 20)
                     (RegisterName $ fromIntegral $ (w .&. 0x000F0000) `shiftR` 15)
                     (RegisterName $ fromIntegral $ (w .&. 0x0000F000) `shiftR` 11)
                     (parseOperand2 (Immediate $ w `testBit` 25) w))
  | w .&. 0x0FB00FF0 == 0x01000090 = Right (getCondition w, readSingleDataSwap w) -- Single data swap
  | otherwise =
    case w .&. 0x0E000000 of -- Test the identity bits
      0x00 -> if (w .&. 0x010000F0) == 0x90 then Right (getCondition w, readGeneralMultiply w) -- multiply
              else Right (getCondition w, readHalfWordDataTransfer w)-- halfword data transfer
      0x08000000 -> undefined -- Block data transfer
      0x0A000000 -> Right (getCondition w, readBranch w) -- Branch instruction
      0x0C000000 -> undefined -- Coprocessor data transfer
      0x0E000000 -> undefined -- Coprocessor data operation
      x | x == 0x6000000 || x == 0x4000000 -> Right (getCondition w, readLoadStore w) -- Load/Store
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

readBranchExchange :: MWord -> Instruction ARM
readBranchExchange br = BranchExchange $ RegisterName $ fromIntegral val
  where
    val = br .&. 0b1111

readBranch :: MWord -> Instruction ARM
readBranch br = Branch linkBit offset
  where
    linkBit = testBit br 24
    offset = br .&. 0xFFFFFF

-- Detect whether it is a Multiply or a Multiply long
readGeneralMultiply :: MWord -> Instruction ARM
readGeneralMultiply instr 
  | isMulLong = readMultiplyLong instr
  | otherwise = readMultiply instr
  where
    isMulLong = testBit instr 23


readMultiply :: MWord -> Instruction ARM
readMultiply instr = 
  Multiply accumulate (SetCondition setCondition) (RegisterName $ fromIntegral dest) (RegisterName $ fromIntegral operand1) 
    (RegisterName $ fromIntegral operand2) (RegisterName $ fromIntegral operand3)
  where
    accumulate = testBit instr 21
    setCondition = testBit instr 20
    dest = (instr .&. 0xF0000) `shiftR` 16
    operand1 = (instr .&. 0xF000) `shiftR` 12
    operand2 = (instr .&. 0xF00) `shiftR` 8
    operand3 = (instr .&. 0xF)

readMultiplyLong :: MWord -> Instruction ARM
readMultiplyLong instr =
  MultiplyLong signed accumulate (SetCondition setCondition) (RegisterName $ fromIntegral destHi) (RegisterName $ fromIntegral destLo) (RegisterName $ fromIntegral operand1) (RegisterName $ fromIntegral operand2)
  where
    signed = testBit instr 22
    accumulate = testBit instr 21
    setCondition = testBit instr 20
    destHi = (instr .&. 0xF0000) `shiftR` 16
    destLo = (instr .&. 0xF000) `shiftR` 12
    operand1 = (instr .&. 0xF00) `shiftR` 8
    operand2 = (instr .&. 0xF)

readSingleDataSwap :: MWord -> Instruction ARM
readSingleDataSwap instr = SingleDataSwap granularity (RegisterName $ fromIntegral base) (RegisterName $ fromIntegral dest) (RegisterName $ fromIntegral src)
  where
    granularity
      | testBit instr 22 = Byte
      | otherwise = Word
    base = (instr .&. 0xF0000) `shiftR` 16
    dest = (instr .&. 0xF000) `shiftR` 12
    src = (instr .&. 0xF)

-- Actually a halfword or signed data transfer but that wouldn't make a nice function name
readHalfWordDataTransfer :: MWord -> Instruction ARM
readHalfWordDataTransfer instr
  | testBit instr 22 = 
    HalfwordDataTransferImmediate preIndex upDown writeBack load signed granularity base dest offsetImmediate
  | otherwise = 
    HalfwordDataTransferRegister preIndex upDown writeBack load signed granularity base dest offset
  where
    preIndex 
      | testBit instr 24 = Pre
      | otherwise = Post
    upDown 
      | testBit instr 23 = Up
      | otherwise = Down
    writeBack = testBit instr 21
    load
      | testBit instr 20 = Load
      | otherwise = Store
    base = RegisterName $ fromIntegral $ (instr .&. 0xF0000) `shiftR` 16
    dest = RegisterName $ fromIntegral $ (instr .&. 0xF000) `shiftR` 12
    offset = RegisterName $ fromIntegral $ (instr .&. 0xF)
    (granularity, signed) = case (instr .&. 0x60) `shiftR` 5 of
      0 -> (Byte, False) -- unsigned byte swap instruction
      1 -> (HalfWord, False) -- Unsigned halfwords
      2 -> (Byte, True) -- signed byte
      3 -> (HalfWord, True) -- Signed halfword 
    offsetImmediate = ((instr .&. 0xF00 )`shiftR` 4) .|. (instr .&. 0xF)

readLoadStore :: MWord -> Instruction ARM
readLoadStore instr = SingleDataTransfer prePost upDown granularity writeBack loadStore base dest offset
  where
    prePost
      | testBit instr 24 = Pre
      | otherwise = Post
    upDown
      | testBit instr 23 = Up
      | otherwise = Down
    granularity
      | testBit instr 22 = Byte
      | otherwise = Word
    writeBack = testBit instr 21
    loadStore
      | testBit instr 20 = Load
      | otherwise = Store
    base = RegisterName $ fromIntegral $ (instr .&. 0xF0000) `shiftR` 16
    dest = RegisterName $ fromIntegral $ (instr .&. 0xF000) `shiftR` 12
    offset 
      | testBit instr 25 = Left $ parseShiftedRegister instr -- Shifted register
      | otherwise = Right (instr .&. 0xFFF) --immediate offset

parseOperand2 :: Immediate -> MWord -> Either (Shifted RegisterName) (Rotated Byte)
parseOperand2 (Immediate False) w =
  Left $ parseShiftedRegister w
parseOperand2 (Immediate True) w =
  Right $ Rotated (fromIntegral $ w .&. 0xF00 `shiftR` 8) (fromIntegral w)

parseShiftedRegister :: MWord -> Shifted RegisterName
parseShiftedRegister w =
  case w `testBit` 4 of
    True ->
      RegisterShift (RegisterName $ fromIntegral $ w .&. 0xF00 `shiftR` 8) shiftType registerName
    False ->
      AmountShift (fromIntegral $ w .&. 0xF80 `shiftR` 7) shiftType registerName
  where
    registerName = RegisterName $ fromIntegral $ w .&. 0xF
    shiftType = fromMaybe (error "Undefined shift type") $
      shiftTypeFromByte $ fromIntegral $ w .&. 0x60 `shiftR` 5
