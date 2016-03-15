module Emulator.CPU.Instructions.ThumbParser where

import Emulator.CPU
import Emulator.Types
import Emulator.CPU.Instructions.Parser
import Utilities.Parser.TemplateHaskell

import Data.Bits
import Numeric (showHex)

parseTHUMB :: HalfWord -> Either String (Instruction THUMB)
parseTHUMB w = case greaterId of
  0 -> if addId then Right $ readAddSub w else Right $ readMovShifted w
  1 -> Right $ readMovCmpAddSub w
  2 -> if ahId then 
        if testBit w 10 then Right $ readHighRegOperation w 
        else Right $ readALUOperation w
       else if relLoadId then Right $ readPCRelativeLoad w
            else if testBit w 9 then Right $ readLoadStoreRegOffset w 
                 else Right $ readLoadStoreSignExtByteHalfWord w
  3 -> Right $ readLoadStoreImmedOffset w
  4 -> if modifierBit then Right $ readSPRelativeLoadStore w else Right $ readLoadStoreHalfword w
  5 -> if not modifierBit then Right $ readLoadAddress w
       else if testBit w 10 then Right $ readPushPopRegisters w
            else Right $ readAddOffsetToSP w
  6 -> if not modifierBit then Right $ readMultipleLoadStore w
       else if cond == 0xF then Right $ readThumbSoftwareInterrupt w
            else Right $ readConditionalBranch w
  7 -> if modifierBit then Right $ readLongBranchWithLink w
       else Right $ readUnconditionalBranch w
  _ -> error ("Undefined opcode: 0x" ++ (showHex w ""))
  where
    greaterId = $(bitmask 15 13) w 
    lesserId = $(bitmask 12 11) w
    modifierBit = testBit w 12
    addId = lesserId == 3
    ahId = lesserId == 0  -- alu or high-reg operation
    relLoadId = lesserId == 1
    cond = $(bitmask 11 8) w

readAddSub :: HalfWord -> Instruction THUMB
readAddSub w = if testBit w 10 then AddSubtractImmediate op (fromIntegral val) srcReg destReg -- Immediate value
               else AddSubtractRegister op (RegisterName $ fromIntegral val) srcReg destReg -- Register value
  where
    op = if testBit w 9 then Subtract else Add
    val = $(bitmask 8 6) w
    srcReg = RegisterName $ fromIntegral $ $(bitmask 5 3) w
    destReg = RegisterName $ fromIntegral $ w .&. 0b111

readMovShifted :: HalfWord -> Instruction THUMB
readMovShifted w = MoveShiftedRegister shifted dest 
  where
    op = $(bitmask 12 11) w
    operation :: ShiftType
    operation
      | op == 0 = LogicalLeft
      | op == 1 = LogicalRight
      | op == 2 = ArithRight
      | otherwise = undefined
    shifted = AmountShift (fromIntegral op) operation $ RegisterName $ fromIntegral $ $(bitmask 5 3) w
    dest = RegisterName $ fromIntegral $ w .&. 0b111

readMovCmpAddSub :: HalfWord -> Instruction THUMB
readMovCmpAddSub w = MovCmpAddSubImmediate opcode op1 $ fromIntegral immediate
  where
    opcode = case $(bitmask 12 11) w of
      0 -> MOV
      1 -> CMP
      2 -> ADD
      3 -> SUB
      _ -> undefined
    op1 = RegisterName $ fromIntegral $ $(bitmask 10 8) w
    immediate = w .&. 0xFF

readHighRegOperation :: HalfWord -> Instruction THUMB
readHighRegOperation = undefined

readALUOperation :: HalfWord -> Instruction THUMB
readALUOperation = undefined

readLoadStoreRegOffset :: HalfWord -> Instruction THUMB
readLoadStoreRegOffset = undefined

readLoadStoreSignExtByteHalfWord :: HalfWord -> Instruction THUMB
readLoadStoreSignExtByteHalfWord = undefined

readPCRelativeLoad :: HalfWord -> Instruction THUMB
readPCRelativeLoad = undefined

readLoadStoreImmedOffset :: HalfWord -> Instruction THUMB
readLoadStoreImmedOffset = undefined

readSPRelativeLoadStore :: HalfWord -> Instruction THUMB
readSPRelativeLoadStore = undefined

readLoadStoreHalfword :: HalfWord -> Instruction THUMB
readLoadStoreHalfword = undefined

readLoadAddress :: HalfWord -> Instruction THUMB
readLoadAddress = undefined

readPushPopRegisters :: HalfWord -> Instruction THUMB
readPushPopRegisters = undefined

readAddOffsetToSP :: HalfWord -> Instruction THUMB
readAddOffsetToSP = undefined

readMultipleLoadStore :: HalfWord -> Instruction THUMB
readMultipleLoadStore = undefined

readThumbSoftwareInterrupt :: HalfWord -> Instruction THUMB
readThumbSoftwareInterrupt = undefined

readConditionalBranch :: HalfWord -> Instruction THUMB
readConditionalBranch = undefined

readLongBranchWithLink :: HalfWord -> Instruction THUMB
readLongBranchWithLink = undefined

readUnconditionalBranch :: HalfWord -> Instruction THUMB
readUnconditionalBranch = undefined
