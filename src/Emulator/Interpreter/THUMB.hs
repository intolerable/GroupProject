module Emulator.Interpreter.THUMB where

import Emulator.CPU hiding (CPUMode(..), Interrupt(..))
import Emulator.CPU.Instructions.THUMB
import qualified Emulator.CPU.Instructions.THUMB.Opcodes as Op
import Emulator.CPU.Instructions.Types
import Emulator.Interpreter.Monad
import Emulator.Types

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
handleMoveShiftedRegister sr r = (registers.rn r) <~ use (registers.shiftedRegisterLens sr)

handleAddSubtractImmediate :: IsSystem s m => AddSub -> Offset -> RegisterName -> RegisterName -> m ()
handleAddSubtractImmediate  as immed dest source = do
  sVal <- use (registers.rn source)
  let result = (addSubToOperator as) sVal immed
  registers.rn dest .= result
  flags.zero .= (result == 0)
  flags.negative .= testBit result 15
  flags.carry .= not (isUnsignedOverflow (addSubToOperator as) [fromIntegral sVal, fromIntegral immed] $ fromIntegral result)
  flags.overflow .= (isSignedOverflow (addSubToOperator as) [fromIntegral sVal, fromIntegral immed] $ fromIntegral result)

handleAddSubtractRegister :: IsSystem s m => AddSub -> RegisterName -> RegisterName -> RegisterName -> m ()
handleAddSubtractRegister as offset src dest = do
  sVal <- use (registers.rn src)
  oVal <- use (registers.rn offset)
  let result = (addSubToOperator as) sVal oVal
  registers.rn dest .= result
  flags.zero .= (result == 0)
  flags.negative .= testBit result 15
  flags.carry .= not (isUnsignedOverflow (addSubToOperator as) [fromIntegral sVal, fromIntegral oVal] $ fromIntegral result)
  flags.overflow .= (isSignedOverflow (addSubToOperator as) [fromIntegral sVal, fromIntegral oVal] $ fromIntegral result)

handleMovCmpAddSubImmediate :: IsSystem s m => Opcode -> RegisterName -> Offset -> m ()
handleMovCmpAddSubImmediate op src immed = case op of
  MOV -> do
    registers.rn src .= (immed .&. 0xFF)
    flags.zero .= (immed == 0)
    flags.negative .= testBit (immed .&. 0xFF) 15
    -- This instruction shouldn't actually touch the carry and overflow flags... I think
  CMP -> do
    let val = immed .&. 0xFF
    sVal <- use (registers.rn src)
    let result = sVal - val
    flags.zero .= (result == 0)
    flags.negative .= testBit result 15
    flags.carry .= not (isUnsignedOverflow (-) [fromIntegral sVal, fromIntegral val] $ fromIntegral result)
    flags.overflow .= (isSignedOverflow (-) [fromIntegral sVal, fromIntegral val] $ fromIntegral result)
  ADD -> undefined
  SUB -> undefined
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

handlePCRelativeLoad :: Monad m => RegisterName -> Offset -> SystemT m ()
handlePCRelativeLoad = undefined

handleThumbLoadStoreRegisterOffset :: Monad m => LoadStore -> Granularity 'Full -> RegisterName -> RegisterName -> RegisterName -> SystemT m ()
handleThumbLoadStoreRegisterOffset = undefined

handleThumbLoadStoreSignExtHalfwordByte :: Monad m => Granularity 'Lower -> LoadStore -> SignExtended -> RegisterName -> RegisterName -> RegisterName -> SystemT m ()
handleThumbLoadStoreSignExtHalfwordByte = undefined

handleThumbLoadStoreImmediateOffset :: Monad m => Granularity 'Full -> LoadStore -> Offset -> RegisterName -> RegisterName -> SystemT m ()
handleThumbLoadStoreImmediateOffset = undefined

handleThumbLoadStoreHalfword :: Monad m => LoadStore -> Offset -> RegisterName -> RegisterName -> SystemT m ()
handleThumbLoadStoreHalfword = undefined

handleSPRelativeLoadStore :: Monad m => LoadStore -> RegisterName -> Offset -> SystemT m ()
handleSPRelativeLoadStore = undefined

handleLoadAddress :: Monad m => BaseSource -> RegisterName -> Offset -> SystemT m ()
handleLoadAddress = undefined

handleSPAddOffset :: Monad m => OffsetDirection -> Offset -> SystemT m ()
handleSPAddOffset = undefined

handlePushPopRegs :: Monad m => LoadStore -> StoreLR -> RegisterList -> SystemT m ()
handlePushPopRegs = undefined

handleMultipleLoadStore :: Monad m => LoadStore -> RegisterName -> RegisterList -> SystemT m ()
handleMultipleLoadStore = undefined

handleConditionalBranch :: Monad m => Condition -> Offset -> SystemT m ()
handleConditionalBranch = undefined

handleThumbSoftwareInterrupt :: Monad m => Value -> SystemT m ()
handleThumbSoftwareInterrupt = error "Unimplemented instruction: Thumb software interrupt"

handleThumbBranch :: IsSystem s m => BranchOffset -> m ()
handleThumbBranch off = do
  pc <- use $ registers.r15
  let val = (fromIntegral pc) + off
  registers.r15 .= fromIntegral (val + 4)

handleLongBranchWLink :: Monad m => LowHigh -> Offset -> SystemT m ()
handleLongBranchWLink = undefined

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
