module Emulator.Interpreter.THUMB where

import Emulator.CPU hiding (CPUMode(..), Interrupt(..))
import Emulator.CPU.Instructions
import Emulator.CPU.Instructions.Flags
import Emulator.Interpreter.ARM (readBlocks, writeBlocks, directionToOperator)
import Emulator.Interpreter.Monad
import Emulator.Memory
import Emulator.Types
import Utilities.Bits
import Utilities.Parser.TemplateHaskell
import qualified Emulator.CPU.Instructions.THUMB.Opcodes as Op

import Control.Lens hiding (op)
import Data.Bits
import Data.Int

interpretThumb :: Monad m => THUMBInstruction -> SystemT m ()
interpretThumb instr =
  case instr of
    MoveShiftedRegister s r ->
      handleMoveShiftedRegister s r
    AddSubtractImmediate as r r' r'' ->
      handleAddSubtractImmediate as r r' r''
    AddSubtractRegister as r r' r'' ->
      handleAddSubtractRegister as r r' r''
    MovCmpAddSubImmediate op r offset ->
      handleMovCmpAddSubImmediate op r offset
    ALUOperation op r r' ->
      handleALUOperation op r r'
    HiRegOperation op r r' ->
      handleHiRegOperation op r r'
    ThumbBranchExchange r ->
      handleThumbBranchExchange r
    PCRelativeLoad r offset ->
      handlePCRelativeLoad r offset
    ThumbLoadStoreRegisterOffset ls g r r' r'' ->
      handleThumbLoadStoreRegisterOffset ls g r r' r''
    ThumbLoadStoreSignExtHalfwordByte g ls s r r' r'' ->
      handleThumbLoadStoreSignExtHalfwordByte g ls s r r' r''
    ThumbLoadStoreImmediateOffset g ls offset r r' ->
      handleThumbLoadStoreImmediateOffset g ls offset r r'
    ThumbLoadStoreHalfword ls offset r r' ->
      handleThumbLoadStoreHalfword ls offset r r'
    SPRelativeLoadStore ls r offset ->
      handleSPRelativeLoadStore ls r offset
    LoadAddress base r offset ->
      handleLoadAddress base r offset
    SPAddOffset ud offset ->
      handleSPAddOffset ud offset
    PushPopRegs ls s r ->
      handlePushPopRegs ls s r
    MultipleLoadStore ls r rlist ->
      handleMultipleLoadStore ls r rlist
    ConditionalBranch cond offset ->
      handleConditionalBranch cond offset
    ThumbSoftwareInterrupt val ->
      handleThumbSoftwareInterrupt val
    ThumbBranch offset ->
      handleThumbBranch offset
    LongBranchWLink lh offset ->
      handleLongBranchWLink lh offset

handleMoveShiftedRegister :: IsSystem s m => Shifted RegisterName -> RegisterName -> m ()
handleMoveShiftedRegister (AmountShift b t base) r = do
  val <- use $ registers.rn base
  c <- use $ flags.carry
  let (newCarry, res) = applyShiftTypeWithCarry t val (fromIntegral b) c
  registers.rn r .= res
  setFlagsLogic res
  flags.carry .= newCarry
handleMoveShiftedRegister (RegisterShift nr t vr) dest = do
  n <- use $ registers.rn nr
  v <- use $ registers.rn vr
  c <- use $ flags.carry
  let (newCarry, res) = applyShiftTypeWithCarry t v (fromIntegral n) c
  setFlagsLogic res
  flags.carry .= newCarry
  registers.rn dest .= res

handleAddSubtractImmediate :: IsSystem s m => AddSub -> Offset -> RegisterName -> RegisterName -> m ()
handleAddSubtractImmediate as immed source dest = do
  sVal <- use (registers.rn source)
  let result = (addSubToOperator as) sVal immed
  registers.rn dest .= result
  setFlagsLogic result
  flags.carry .= wouldCarry (addSubToOperator as) (fromIntegral sVal) (fromIntegral immed)
  flags.overflow .= isOverflow result

handleAddSubtractRegister :: IsSystem s m => AddSub -> RegisterName -> RegisterName -> RegisterName -> m ()
handleAddSubtractRegister as offset src dest = do
  sVal <- use (registers.rn src)
  oVal <- use (registers.rn offset)
  let result = (addSubToOperator as) sVal oVal
  registers.rn dest .= result
  setFlagsLogic result
  flags.carry .= wouldCarry (addSubToOperator as) (fromIntegral sVal) (fromIntegral oVal)
  flags.overflow .= isOverflow result

handleMovCmpAddSubImmediate :: IsSystem s m => Opcode -> RegisterName -> Offset -> m ()
handleMovCmpAddSubImmediate op src immed = case op of
  MOV -> do
    let imVal = immed .&. 0xFF
    registers.rn src .= imVal
    flags.zero .= (immed == 0)
    flags.negative .= testBit imVal 15
  CMP -> do
    let val = immed .&. 0xFF
    sVal <- use (registers.rn src)
    let result = sVal - val
    setFlagsLogic result
    flags.carry .= wouldCarry (-) (fromIntegral sVal) (fromIntegral val)
    flags.overflow .= isOverflow result
  ADD -> do
    let imVal = immed .&. 0xFF
    sVal <- use $ registers.rn src
    let res = sVal + imVal
    registers.rn src .= res
    setFlagsLogic res
    flags.carry .= wouldCarry (+) (fromIntegral sVal) (fromIntegral imVal)
    flags.overflow .= isOverflow res
  SUB -> do
    let imVal = immed .&. 0xFF
    sVal <- use $ registers.rn src
    let res = sVal - imVal
    registers.rn src .= res
    setFlagsLogic res
    flags.carry .= wouldCarry (-) (fromIntegral sVal) (fromIntegral imVal)
    flags.overflow .= isOverflow res
  _ -> error "Incorrect arguments passed to THUMB MovCmpAddSubImmediate function."

handleALUOperation :: IsSystem s m => ThumbOpcode -> RegisterName -> RegisterName -> m ()
handleALUOperation opcode src dest =
  (Op.functionFromOpcode opcode) src dest

handleHiRegOperation :: IsSystem s m => ThumbOpcode -> RegisterName -> RegisterName -> m ()
handleHiRegOperation op src dest = do
  v <- use $ registers.rn src
  v' <- use $ registers.rn dest
  case op of
    T_ADD -> registers.rn dest .= v + v'
    T_CMP -> (Op.functionFromOpcode T_CMP) src dest
    T_MOV -> registers.rn dest .= v
    _ -> error "Unsupported operation in THUMB hiRegOperaton"

handleThumbBranchExchange :: IsSystem s m => RegisterName -> m ()
handleThumbBranchExchange r = do
  addr <- use $ registers.rn r
  let realAddr = clearBit addr 0
  registers.r15 .= (realAddr + 4)
  flags.thumbStateBit .= addr `testBit` 0

handlePCRelativeLoad :: IsSystem s m => RegisterName -> Offset -> m ()
handlePCRelativeLoad dest off = do
  pc' <- use $ registers.pc
  let addr = pc' + off
  word <- readAddressWord (addr .&. 0xFFFFFFC)
  registers.rn dest .= word

handleThumbLoadStoreRegisterOffset :: IsSystem s m => LoadStore -> Granularity 'Full -> RegisterName -> RegisterName -> RegisterName -> m ()
handleThumbLoadStoreRegisterOffset ls g offR baseR destR = do
  offset <- use $ registers.rn offR
  base <- use $ registers.rn baseR
  dest <- use $ registers.rn destR
  let addr = base + offset
  if g == Word  then if ls == Load then registers.rn destR <~ readAddressWord addr
                                   else writeAddressWord addr dest
                else if ls == Load then do
                                      val <- readAddressWord addr
                                      registers.rn destR .= (val .&. 0xFF)
                                   else writeAddressWord addr (dest .&. 0xFF)

handleThumbLoadStoreSignExtHalfwordByte :: IsSystem s m => Granularity 'Lower -> LoadStore -> SignExtended -> RegisterName -> RegisterName -> RegisterName -> m ()
handleThumbLoadStoreSignExtHalfwordByte g ls se offR baseR destR = do
  offset <- use $ registers.rn offR
  base <- use $ registers.rn baseR
  dest <- use $ registers.rn destR
  let addr = offset + base
  case (g, ls, se) of
    (Byte, Load, True) -> do
      val <- readAddressHalfWord addr
      (registers.rn destR) .= byteExtend (fromIntegral (val .&. 0xFF))
    (HalfWord, Load, False) -> do
      val <- readAddressHalfWord addr
      (registers.rn destR) .= fromIntegral (val .&. 0xFFFF)
    (HalfWord, Store, False) -> writeAddressHalfWord addr $ fromIntegral (dest .&. 0xFFFF)
    (HalfWord, Load, True) -> do
      val <- readAddressHalfWord addr
      (registers.rn destR) .= halfWordExtend (fromIntegral val)
    _ -> error $ "Incorrect parameters passed to LoadStoreSignExtHalfWordByte: " ++ show (g, ls, se)

handleThumbLoadStoreImmediateOffset :: IsSystem s m => Granularity 'Full -> LoadStore -> Offset -> RegisterName -> RegisterName -> m ()
handleThumbLoadStoreImmediateOffset g ls offset baseR destR = do
  base <- use $ registers.rn baseR
  let addr = base + (case g of { Byte -> offset; Word -> offset `shiftL` 2 })
  case (g, ls) of
    (Byte, Load) -> do
      byte <- readAddressByte addr
      registers.rn destR .= fromIntegral byte
    (Byte, Store) -> do
      val <- use $ registers.rn destR
      writeAddressByte addr $ fromIntegral (val .&. 0xFF)
    (Word, Load) -> registers.rn destR <~ readAddressWord addr
    (Word, Store) -> do
      val <- use $ registers.rn destR
      writeAddressWord addr val

handleThumbLoadStoreHalfword :: IsSystem s m => LoadStore -> Offset -> RegisterName -> RegisterName -> m ()
handleThumbLoadStoreHalfword ls offset baseR destR = do
  base <- use $ registers.rn baseR
  let addr = base + offset
  case ls of
    Load -> registers.rn destR <~ fromIntegral <$> readAddressHalfWord addr
    Store -> do
      val <- use $ registers.rn destR
      writeAddressHalfWord addr $ fromIntegral (val .&. 0xFFFF)

handleSPRelativeLoadStore :: IsSystem s m => LoadStore -> RegisterName -> Offset -> m ()
handleSPRelativeLoadStore ls destR offset = do
  -- sp is register 13
  base <- use $ registers.r13
  let addr = base + offset
  case ls of
    Store -> do
      val <- use $ registers.rn destR
      writeAddressWord addr val
    Load -> registers.rn destR <~ readAddressWord addr

handleLoadAddress :: IsSystem s m => BaseSource -> RegisterName -> Offset -> m ()
handleLoadAddress PC destR offset = do
  val <- use $ registers.r15
  let base = val .&. 0xFFFFFFFD
  let addr = base + offset
  registers.rn destR .= addr
handleLoadAddress SP destR offset = do
  base <- use $ registers.r13
  let addr = base + offset
  registers.rn destR .= addr

handleSPAddOffset :: IsSystem s m => OffsetDirection -> Offset -> m ()
handleSPAddOffset ud offset =
  registers.sp %= \x -> directionToOperator ud x offset

handlePushPopRegs :: IsSystem s m => LoadStore -> StoreLR -> RegisterList -> m ()
handlePushPopRegs ls st rlist =
  case ls of
    -- Read blocks of memory, from the stack, shrinking the stack upwards
    Load -> do
      let rlist' = if st then rlist ++ [RegisterName 15] else rlist
      mapM_ popFromStack rlist'
    Store -> do
      let rlist' = if st then rlist ++ [RegisterName 14] else rlist
      mapM_ pushToStack rlist'

pushToStack :: IsSystem s m => RegisterName -> m ()
pushToStack r = do
  currentStackPointer <- use (registers.sp)
  registerValue <- use (registers.rn r)
  writeAddressWord currentStackPointer registerValue
  registers.sp -= 4

popFromStack :: IsSystem s m => RegisterName -> m ()
popFromStack r = do
  registers.sp += 4
  currentStackPointer <- use (registers.sp)
  registers.rn r <~ readAddressWord currentStackPointer

handleMultipleLoadStore :: IsSystem s m => LoadStore -> RegisterName -> RegisterList -> m ()
handleMultipleLoadStore ls baseR rlist = do
  base <- use $ registers.rn baseR
  registers.rn baseR <~ (case ls of
    Load -> readBlocks Up base rlist
    Store -> writeBlocks Up base rlist)

handleConditionalBranch :: IsSystem s m => Condition -> Offset -> m ()
handleConditionalBranch cond off =
  conditionally cond $
    registers.r15 %= \x -> fromIntegral $ (fromIntegral x) + off + 2

handleThumbSoftwareInterrupt :: Monad m => Value -> SystemT m ()
handleThumbSoftwareInterrupt = error "Unimplemented instruction: Thumb software interrupt"

handleThumbBranch :: IsSystem s m => BranchOffset -> m ()
handleThumbBranch off = do
  pc' <- use $ registers.pc
  let val = (fromIntegral pc') + off
  registers.pc .= fromIntegral (val + 2)

handleLongBranchWLink :: IsSystem s m => LowHigh -> HalfWord -> m ()
handleLongBranchWLink High off = do
  let offset = (fromIntegral ((fromIntegral off :: MWord) `shiftL` 21) :: Int32) `shiftR` 9
  oldPC <- use (registers.pc)
  registers.lr .= fromIntegral (fromIntegral oldPC + offset)
handleLongBranchWLink Low off = do
  let offset = fromIntegral off `shiftL` 1 :: Int32
  oldLR <- use (registers.lr)
  oldPC <- use (registers.pc)
  registers.pc .= fromIntegral (fromIntegral oldLR + offset + 2)
  registers.lr .= ((oldPC + 2) `setBit` 0)

addSubToOperator :: Num a => AddSub -> (a -> a -> a)
addSubToOperator Add = (+)
addSubToOperator Subtract = (-)

isUnsignedOverflow :: a ~ Integer => (a -> a -> a) -> [HalfWord] -> HalfWord -> Bool
isUnsignedOverflow f args result = (fromIntegral result) /= preciseResult
  where
    preciseResult = foldl1 f $ map fromIntegral args :: Integer

isSignedOverflow :: (a ~ Int16) => (a -> a -> a) -> [HalfWord] -> HalfWord -> Bool
isSignedOverflow f args result = (fromIntegral signedResult :: Integer) /= (fromIntegral result :: Integer)
  where
    signedResult = foldl1 f $ map fromIntegral args :: Int16
