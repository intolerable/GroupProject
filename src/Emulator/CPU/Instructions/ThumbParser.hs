module Emulator.CPU.Instructions.ThumbParser where

import Emulator.Types
import Emulator.CPU.Instructions.Parser
import Utilities.Parser.TemplateHaskell

import Data.Bits
import Numeric (showHex)

parseTHUMB :: HalfWord -> Either String (Instruction THUMB)
parseTHUMB w = case greaterId of
  0 -> if addId then Right $ readAdd w else Right $ readMovShifted w
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

readAdd :: HalfWord -> Instruction THUMB
readAdd = undefined

readMovShifted :: HalfWord -> Instruction THUMB
readMovShifted = undefined

readMovCmpAddSub :: HalfWord -> Instruction THUMB
readMovCmpAddSub = undefined

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
