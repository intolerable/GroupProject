module Emulator.CPU.Instructions.THUMB.Parser where

import Emulator.CPU
import Emulator.CPU.Instructions.ARM.Parser (parseRegisterList)
import Emulator.CPU.Instructions.THUMB
import Emulator.CPU.Instructions.Types
import Emulator.Types
import Utilities.Bits
import Utilities.Parser.TemplateHaskell
import Utilities.Show

import Data.Bits

parseTHUMB :: HalfWord -> Either String THUMBInstruction
parseTHUMB w = case greaterId of
  0 -> if addId then Right $ readAddSub w else Right $ readMovShifted w
  1 -> Right $ readMovCmpAddSub w
  2 ->
    if ahId
      then
        if testBit w 10
          then Right $ readHighRegOperation w
          else Right $ readALUOperation w
       else
        if relLoadId
          then Right $ readPCRelativeLoad w
          else
            if testBit w 9
              then Right $ readLoadStoreRegOffset w
              else Right $ readLoadStoreSignExtByteHalfWord w
  3 -> Right $ readLoadStoreImmedOffset w
  4 -> if modifierBit then Right $ readSPRelativeLoadStore w else Right $ readLoadStoreHalfword w
  5 ->
    if not modifierBit
      then Right $ readLoadAddress w
      else
        if testBit w 10
          then Right $ readPushPopRegisters w
          else if not $ testBit w 11 then Right $ readAddOffsetToSP w
                                     else error $ "Undefined instruction: " ++ showHex w
  6 ->
    if not modifierBit
      then Right $ readMultipleLoadStore w
      else
        if cond == 0xF
          then Right $ readThumbSoftwareInterrupt w
          else Right $ readConditionalBranch w
  7 -> if modifierBit then Right $ readLongBranchWithLink w else Right $ readUnconditionalBranch w
  _ -> error $ "Undefined opcode: " ++ showHex w
  where
    greaterId = $(bitmask 15 13) w
    lesserId = $(bitmask 12 11) w
    modifierBit = testBit w 12
    addId = lesserId == 3
    ahId = lesserId == 0 -- alu or high-reg operation
    relLoadId = lesserId == 1
    cond = $(bitmask 11 8) w

readAddSub :: HalfWord -> THUMBInstruction
readAddSub w =
  if testBit w 10
    then AddSubtractImmediate op (fromIntegral val) srcReg destReg -- Immediate value
    else AddSubtractRegister op (RegisterName $ fromIntegral val) srcReg destReg -- Register value
  where
    op = if testBit w 9 then Subtract else Add
    val = $(bitmask 8 6) w
    srcReg = RegisterName $ fromIntegral $ $(bitmask 5 3) w
    destReg = RegisterName $ fromIntegral $ w .&. 0b111

readMovShifted :: HalfWord -> THUMBInstruction
readMovShifted w = MoveShiftedRegister shifted dest
  where
    op = $(bitmask 12 11) w
    operation :: ShiftType
    operation
      | op == 0 = LogicalLeft
      | op == 1 = LogicalRight
      | op == 2 = ArithRight
      | otherwise = error "readMovShifted: invalid shift type"
    shifted = AmountShift (fromIntegral shiftVal) operation $ RegisterName $ fromIntegral $ $(bitmask 5 3) w
    shiftVal = $(bitmask 10 6) w
    dest = RegisterName $ fromIntegral $ w .&. 0b111

readMovCmpAddSub :: HalfWord -> THUMBInstruction
readMovCmpAddSub w = MovCmpAddSubImmediate opcode op1 $ fromIntegral immediate
  where
    opcode = case $(bitmask 12 11) w of
      0 -> MOV
      1 -> CMP
      2 -> ADD
      3 -> SUB
      _ -> error "readMovCmpAddSub: invalid opcode"
    op1 = RegisterName $ fromIntegral $ $(bitmask 10 8) w
    immediate = w .&. 0xFF

readHighRegOperation :: HalfWord -> THUMBInstruction
readHighRegOperation w = case opcode of
  0 -> HiRegOperation T_ADD src dest
  1 -> HiRegOperation T_CMP src dest
  2 -> HiRegOperation T_MOV src dest
  3 -> readHighRegBX w
  _ -> error "Undefined opcode"
  where
    opcode = $(bitmask 9 8) w
    src =
      RegisterName $ fromIntegral $ (if testBit w 6 then 8 else 0) + $(bitmask 5 3) w
    dest =
      RegisterName $ fromIntegral $ (if testBit w 7 then 8 else 0) + $(bitmask 2 0) w

readHighRegBX :: HalfWord -> THUMBInstruction
readHighRegBX w = ThumbBranchExchange $ RegisterName $ fromIntegral $ offset + $(bitmask 5 3) w
  where
    offset = if testBit w 6 then 8 else 0

readALUOperation :: HalfWord -> THUMBInstruction
readALUOperation w = case opcode of
  (Just v) -> ALUOperation v srcReg destReg
  Nothing -> error "Undefined opcode"
  where
    opcode = thumbOpcodeFromByte $ fromIntegral $ $(bitmask 9 6) w
    srcReg = RegisterName $ fromIntegral $ $(bitmask 5 3) w
    destReg = RegisterName $ fromIntegral $ w .&. 0b111

readLoadStoreRegOffset :: HalfWord -> THUMBInstruction
readLoadStoreRegOffset w = ThumbLoadStoreRegisterOffset ls granularity offset base dest
  where
    ls = if testBit w 11 then Load else Store
    granularity = if testBit w 10 then Byte else Word
    offset = RegisterName $ fromIntegral $ $(bitmask 8 6) w
    base = RegisterName $ fromIntegral $ $(bitmask 5 3) w
    dest = RegisterName $ fromIntegral $ w .&. 0b111

readLoadStoreSignExtByteHalfWord :: HalfWord -> THUMBInstruction
readLoadStoreSignExtByteHalfWord w = case $(bitmask 11 10) w of
  0 -> -- Store halfword
    ThumbLoadStoreSignExtHalfwordByte HalfWord Store False offset base dest
  1 -> -- Load halfword
    ThumbLoadStoreSignExtHalfwordByte HalfWord Load False offset base dest
  2 -> -- Load sign-extended byte
    ThumbLoadStoreSignExtHalfwordByte Byte Load True offset base dest
  3 -> -- Load sign-extended halfword
    ThumbLoadStoreSignExtHalfwordByte HalfWord Load True offset base dest
  _ -> error "Undefined opcode"
  where
    offset = RegisterName $ fromIntegral $ $(bitmask 8 6) w
    base = RegisterName $ fromIntegral $ $(bitmask 5 3) w
    dest = RegisterName $ fromIntegral $ w .&. 0b111

readPCRelativeLoad :: HalfWord -> THUMBInstruction
readPCRelativeLoad w = PCRelativeLoad dest offset
  where
    dest = RegisterName $ fromIntegral $ $(bitmask 10 8) w
    offset = fromIntegral $ (w .&. 0xFF) `shiftL` 2

readLoadStoreImmedOffset :: HalfWord -> THUMBInstruction
readLoadStoreImmedOffset w =
  ThumbLoadStoreImmediateOffset granularity ls offset base dest
  where
    granularity = if testBit w 12 then Byte else Word
    ls = if testBit w 11 then Load else Store
    offset = fromIntegral $ $(bitmask 10 6) w
    base = RegisterName $ fromIntegral $ $(bitmask 5 3) w
    dest = RegisterName $ fromIntegral $ w .&. 0b111

readSPRelativeLoadStore :: HalfWord -> THUMBInstruction
readSPRelativeLoadStore w = SPRelativeLoadStore ls dest offset
  where
    ls = if testBit w 11 then Load else Store
    dest = RegisterName $ fromIntegral $ $(bitmask 10 8) w
    offset = fromIntegral $ (w .&. 0xFF) `shiftL` 2

readLoadStoreHalfword :: HalfWord -> THUMBInstruction
readLoadStoreHalfword w = ThumbLoadStoreHalfword ls offset base dest
  where
    ls = if testBit w 11 then Load else Store
    offset = fromIntegral $ ($(bitmask 10 6) w) `shiftL` 1
    base = RegisterName $ fromIntegral $ $(bitmask 5 3) w
    dest = RegisterName $ fromIntegral $ w .&. 0b111

readLoadAddress :: HalfWord -> THUMBInstruction
readLoadAddress w = LoadAddress source dest offset
  where
    source = if testBit w 11 then SP else PC
    dest = RegisterName $ fromIntegral $ $(bitmask 10 8) w
    offset = fromIntegral $ (w .&. 0xFF) `shiftL` 2

readPushPopRegisters :: HalfWord -> THUMBInstruction
readPushPopRegisters w = PushPopRegs ls store rlist
  where
    ls = if testBit w 11 then Load else Store
    store = testBit w 8
    rlist = parseRegisterList (fromIntegral $ $(bitmask 7 0) w) 8

readAddOffsetToSP :: HalfWord -> THUMBInstruction
readAddOffsetToSP w = SPAddOffset dir offset
  where
    dir = if testBit w 7 then Down else Up
    offset = fromIntegral $ $(bitmask 6 0) w

readMultipleLoadStore :: HalfWord -> THUMBInstruction
readMultipleLoadStore w = MultipleLoadStore ls base rlist
  where
    ls = if testBit w 11 then Load else Store
    base = RegisterName $ fromIntegral $ $(bitmask 10 8) w
    rlist = parseRegisterList (fromIntegral $ $(bitmask 7 0) w) 8

readThumbSoftwareInterrupt :: HalfWord -> THUMBInstruction
readThumbSoftwareInterrupt = error "readThumbSoftwareInterrupt: unimplemented parser"

readConditionalBranch :: HalfWord -> THUMBInstruction
readConditionalBranch w = ConditionalBranch cond offset
  where
    cond = case maybeCond of
      (Just v) -> v
      Nothing -> error "Error reading condition ThumbParser.hs:210"
    maybeCond = conditionFromByte $ fromIntegral $ $(bitmask 11 8) w
    offset = (fromIntegral $ $(bitmask 7 0) w) `shiftL` 1

readLongBranchWithLink :: HalfWord -> THUMBInstruction
readLongBranchWithLink w =
  LongBranchWLink (if w `testBit` 11 then Low else High) $ $(bitmask 10 0) w

readUnconditionalBranch :: HalfWord -> THUMBInstruction
readUnconditionalBranch w = ThumbBranch offset
  where
    offset = fromIntegral $ (arithExtend (fromIntegral $ $(bitmask 10 0) w) 10) `shiftL` 1
