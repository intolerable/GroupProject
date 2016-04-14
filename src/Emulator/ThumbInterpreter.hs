module Emulator.ThumbInterpreter where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.Interpreter (SystemT)
import Emulator.Types

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

handleMoveShiftedRegister :: Monad m => Shifted RegisterName -> RegisterName -> SystemT m ()
handleMoveShiftedRegister = undefined

handleAddSubtractImmediate :: Monad m => AddSub -> Offset -> RegisterName -> RegisterName -> SystemT m ()
handleAddSubtractImmediate = undefined

handleAddSubtractRegister :: Monad m => AddSub -> RegisterName -> RegisterName -> RegisterName -> SystemT m ()
handleAddSubtractRegister = undefined

handleMovCmpAddSubImmediate :: Monad m => Opcode -> RegisterName -> Offset -> SystemT m ()
handleMovCmpAddSubImmediate = undefined

handleALUOperation :: Monad m => ThumbOpcode -> RegisterName -> RegisterName -> SystemT m ()
handleALUOperation = undefined

handleHiRegOperation :: Monad m => ThumbOpcode -> RegisterName -> RegisterName -> SystemT m ()
handleHiRegOperation = undefined

handleThumbBranchExchange :: Monad m => RegisterName -> SystemT m ()
handleThumbBranchExchange = undefined

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

handleThumbBranch :: Monad m => Offset -> SystemT m ()
handleThumbBranch = undefined

handleLongBranchWLink :: Monad m => LowHigh -> Offset -> SystemT m ()
handleLongBranchWLink = undefined
