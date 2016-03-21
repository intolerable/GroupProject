module Emulator.CPU.Instructions.Parser where

import Emulator.CPU hiding (SoftwareInterrupt)
import Emulator.Types
import Utilities.Parser.TemplateHaskell

import Data.Bits
import Data.Maybe
import Numeric (showHex)

data CPUMode = ARM | THUMB
  deriving (Show, Read, Eq)

type ARM = 'ARM
type THUMB = 'THUMB

newtype SetCondition = SetCondition Bool
  deriving (Show, Read, Eq)

data LoadStore = Load | Store
  deriving (Show, Read, Eq)

data PrePost = Pre | Post
  deriving (Show, Read, Eq)

data UpDown = Up | Down
  deriving (Show, Read, Eq)

data AddSub = Add | Subtract
  deriving (Show, Read, Eq)

data LowHigh = Low | High
  deriving (Show, Read, Eq)

data Granularity = Byte | Word | HalfWord
  deriving (Show, Read, Eq)

newtype Immediate = Immediate Bool
  deriving (Show, Read, Eq)

data Rotated a = Rotated Int a
  deriving (Show, Read, Eq)

data BaseSource = SP | PC
  deriving (Show, Read, Eq)

newtype Link = Link Bool
  deriving (Show, Read, Eq)

type WriteBack = Bool
type Signed = Bool
type Accumulate = Bool
type HighReg = Bool
type SignExtended = Bool
type StoreLR = Bool
type Offset = MWord
type Value = Int -- Value is used for signed values in instructions
type ForceUserMode = Bool
type RegisterList = [RegisterName]

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
  -- Thumb instructions --
  MoveShiftedRegister :: (Shifted RegisterName) -> RegisterName -> Instruction THUMB
  AddSubtractImmediate :: AddSub -> Offset -> RegisterName -> RegisterName -> Instruction THUMB
  AddSubtractRegister :: AddSub -> RegisterName -> RegisterName -> RegisterName -> Instruction THUMB
  -- FIXME: Perhaps don't use Opcode here as the structure supports more operations than the instruction does
  MovCmpAddSubImmediate :: Opcode -> RegisterName -> Offset -> Instruction THUMB
  ALUOperation :: ThumbOpcode -> RegisterName -> RegisterName -> Instruction THUMB
  HiRegOperation :: ThumbOpcode -> RegisterName -> RegisterName -> Instruction THUMB
  ThumbBranchExchange :: RegisterName -> Instruction THUMB
  PCRelativeLoad :: RegisterName -> Offset -> Instruction THUMB
  ThumbLoadStoreRegisterOffset :: LoadStore -> Granularity -> RegisterName -> RegisterName -> RegisterName -> Instruction THUMB
  ThumbLoadStoreSignExtHalfwordByte :: Granularity -> LoadStore -> SignExtended -> RegisterName -> RegisterName -> RegisterName -> Instruction THUMB
  ThumbLoadStoreImmediateOffset :: Granularity -> LoadStore -> Offset -> RegisterName -> RegisterName -> Instruction THUMB
  ThumbLoadStoreHalfword :: LoadStore -> Offset -> RegisterName -> RegisterName -> Instruction THUMB
  SPRelativeLoadStore :: LoadStore -> RegisterName -> Offset -> Instruction THUMB
  LoadAddress :: BaseSource -> RegisterName -> Offset -> Instruction THUMB
  SPAddOffset :: UpDown -> Offset -> Instruction THUMB
  PushPopRegs :: LoadStore -> StoreLR -> RegisterList -> Instruction THUMB
  MultipleLoadStore :: LoadStore -> RegisterName -> RegisterList -> Instruction THUMB
  ConditionalBranch :: Condition -> Value -> Instruction THUMB
  ThumbSoftwareInterrupt :: Value -> Instruction THUMB
  ThumbBranch :: Offset -> Instruction THUMB
  LongBranchWLink :: LowHigh -> Offset -> Instruction THUMB

deriving instance Show (Instruction a)
deriving instance Eq (Instruction a)

parseARM :: MWord -> Either String (Condition, Instruction ARM)
parseARM w
  | w .&. 0x0FFFFFF0 == 0x012FFF10 = Right (getCondition w, readBranchExchange w) -- Definitely branch exchange instruction
  | (w .&. 0x0C000000 == 0x00) && (testBit w 25 || (w .&. 0b10010000) /= 0b10010000) = -- Data Processing thing
    Right (getCondition w,
      DataProcessing (getOpcode w)
                     (SetCondition $ w `testBit` 20)
                     (RegisterName $ fromIntegral $ (w .&. 0x000F0000) `shiftR` 16)
                     (RegisterName $ fromIntegral $ (w .&. 0x0000F000) `shiftR` 12)
                     (parseOperand2 (Immediate $ w `testBit` 25) w))
  | w .&. 0x0FB00FF0 == 0x01000090 = Right (getCondition w, readSingleDataSwap w) -- Single data swap
  | $(bitmask 27 24) w == 0b1111 = Right (getCondition w, SoftwareInterrupt) -- Software interrupt
  | otherwise =
    case w .&. 0x0E000000 of -- Test the identity bits
      0x00 -> if (w .&. 0x010000F0) == 0x90 then Right (getCondition w, readGeneralMultiply w) -- multiply
              else Right (getCondition w, readHalfWordDataTransfer w)-- halfword data transfer
      0x08000000 -> Right (getCondition w, readBlockDataTransfer w) -- Block data transfer
      0x0A000000 -> Right (getCondition w, readBranch w) -- Branch instruction
      0x0C000000 -> error "parseARM: undefined instruction: CoprocessorDataTransfer" -- Coprocessor data transfer
      0x0E000000 -> error "parseARM: undefined instruction: CoprocessorDataOperation" -- Coprocessor data operation
      x | x == 0x6000000 || x == 0x4000000 -> Right (getCondition w, readLoadStore w) -- Load/Store
      _ -> error ("Undefined opcode: 0x" ++ (showHex w ""))

getCondition :: MWord -> Condition
getCondition w =
  case conditionFromByte $ fromIntegral $ $(bitmask 31 28) w of
    Just x -> x
    Nothing -> error $ "getCondition: invalid condition (" ++ show w ++ ")"

getOpcode :: MWord -> Opcode
getOpcode w =
  case opcodeFromByte $ fromIntegral $ $(bitmask 24 21) w of
    Just x -> x
    Nothing -> error $ "getOpcode: invalid opcode (" ++ show w ++ ")"

readBranchExchange :: MWord -> Instruction ARM
readBranchExchange w =
  BranchExchange $ RegisterName $ fromIntegral $ $(bitmask 3 0) w

readBranch :: MWord -> Instruction ARM
readBranch br = Branch linkBit offset
  where
    linkBit = Link $ testBit br 24
    offset = ((br .&. 0xFFFFFF) `shiftL` 2)

-- Detect whether it is a Multiply or a Multiply long
readGeneralMultiply :: MWord -> Instruction ARM
readGeneralMultiply instr =
  if isMulLong then readMultiplyLong instr else readMultiply instr
    where
      isMulLong = testBit instr 23


readMultiply :: MWord -> Instruction ARM
readMultiply instr =
  Multiply accumulate (SetCondition setCondition) (RegisterName $ fromIntegral dest) (RegisterName $ fromIntegral operand1)
    (RegisterName $ fromIntegral operand2) (RegisterName $ fromIntegral operand3)
    where
      accumulate = testBit instr 21
      setCondition = testBit instr 20
      dest = $(bitmask 19 16) instr
      operand1 = $(bitmask 15 12) instr
      operand2 = $(bitmask 11 8) instr
      operand3 = $(bitmask 3 0) instr

readMultiplyLong :: MWord -> Instruction ARM
readMultiplyLong instr =
  MultiplyLong signed
               accumulate
               (SetCondition setCondition)
               (RegisterName $ fromIntegral destHi)
               (RegisterName $ fromIntegral destLo)
               (RegisterName $ fromIntegral operand1)
               (RegisterName $ fromIntegral operand2)
    where
      signed = testBit instr 22
      accumulate = testBit instr 21
      setCondition = testBit instr 20
      destHi = $(bitmask 19 16) instr
      destLo = $(bitmask 15 12) instr
      operand1 = $(bitmask 11 8) instr
      operand2 = $(bitmask 3 0) instr

readSingleDataSwap :: MWord -> Instruction ARM
readSingleDataSwap instr =
  SingleDataSwap granularity
                 (RegisterName $ fromIntegral base)
                 (RegisterName $ fromIntegral dest)
                 (RegisterName $ fromIntegral src)
    where
      granularity = if instr `testBit` 22 then Byte else Word
      base = $(bitmask 19 16) instr
      dest = $(bitmask 15 12) instr
      src = $(bitmask 3 0) instr

-- Actually a halfword or signed data transfer but that wouldn't make a nice function name
readHalfWordDataTransfer :: MWord -> Instruction ARM
readHalfWordDataTransfer instr
  | testBit instr 22 =
    HalfwordDataTransferImmediate preIndex upDown writeBack load signed granularity base dest offsetImmediate
  | otherwise =
    HalfwordDataTransferRegister preIndex upDown writeBack load signed granularity base dest offset
  where
    preIndex = if instr `testBit` 24 then Pre else Post
    upDown = if instr `testBit` 23 then Up else Down
    writeBack = testBit instr 21
    load = if instr `testBit` 20 then Load else Store
    base = RegisterName $ fromIntegral $ (instr .&. 0xF0000) `shiftR` 16
    dest = RegisterName $ fromIntegral $ (instr .&. 0xF000) `shiftR` 12
    offset = RegisterName $ fromIntegral $ (instr .&. 0xF)
    granularity = if instr `testBit` 5 then HalfWord else Byte
    signed = instr `testBit` 6
    offsetImmediate =
      ($(bitmask 11 8) instr `shiftL` 4) .|. $(bitmask 3 0) instr

readLoadStore :: MWord -> Instruction ARM
readLoadStore instr =
  SingleDataTransfer prePost upDown granularity writeBack loadStore base dest offset
    where
      prePost = if instr `testBit` 24 then Pre else Post
      upDown = if instr `testBit` 23 then Up else Down
      granularity = if instr `testBit` 22 then Byte else Word
      writeBack = testBit instr 21
      loadStore = if instr `testBit` 20 then Load else Store
      base = RegisterName $ fromIntegral $ $(bitmask 19 16) instr
      dest = RegisterName $ fromIntegral $ $(bitmask 15 12) instr
      offset = if instr `testBit` 25
        then Left $ parseShiftedRegister instr
        else Right $ $(bitmask 11 0) instr

readBlockDataTransfer :: MWord -> Instruction ARM
readBlockDataTransfer instr =
  BlockDataTransfer prePost upDown forceUser writeBack loadStore base regList
    where
      prePost = if instr `testBit` 24 then Pre else Post
      upDown = if instr `testBit` 23 then Up else Down
      forceUser = testBit instr 22
      writeBack = testBit instr 21
      loadStore = if instr `testBit` 20 then Load else Store
      base = RegisterName $ fromIntegral $ $(bitmask 19 16) instr
      regList = parseRegisterList ($(bitmask 15 0) instr) 16

parseOperand2 :: Immediate -> MWord -> Either (Shifted RegisterName) (Rotated Byte)
parseOperand2 (Immediate False) w =
  Left $ parseShiftedRegister w
parseOperand2 (Immediate True) w =
  Right $ Rotated (fromIntegral $ $(bitmask 11 8) w) (fromIntegral $ $(bitmask 7 0) w)

parseShiftedRegister :: MWord -> Shifted RegisterName
parseShiftedRegister w =
  case w `testBit` 4 of
    True ->
      RegisterShift (RegisterName $ fromIntegral $ $(bitmask 11 8) w) shiftType registerName
    False ->
      AmountShift (fromIntegral $ $(bitmask 11 7) w) shiftType registerName
    where
      registerName = RegisterName $ fromIntegral $ $(bitmask 3 0) w
      shiftType = fromMaybe (error "parseShiftedRegister(shiftType): unknown shift type") $
        shiftTypeFromByte $ fromIntegral $ $(bitmask 6 5) w

parseRegisterList :: MWord -> Int -> RegisterList
parseRegisterList w' m = parseRegisterList' w' 0 []
  where
    parseRegisterList' :: MWord -> Int -> RegisterList -> RegisterList
    parseRegisterList' w n list
      | n == m = list
      | testBit w n = parseRegisterList' w (n+1) $ (RegisterName n) : list
      | otherwise = parseRegisterList' w (n+1) list
