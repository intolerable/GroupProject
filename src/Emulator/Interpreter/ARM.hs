module Emulator.Interpreter.ARM where

import Emulator.CPU hiding (CPUMode(..), Interrupt(..))
import Emulator.CPU.Instructions.ARM
import Emulator.CPU.Instructions.ARM.Opcodes
import Emulator.CPU.Instructions.Types
import Emulator.Interpreter.Monad
import Emulator.Types
import Emulator.Memory
import Utilities.Bits
import Utilities.Parser.TemplateHaskell

import Control.Lens
import Control.Monad
import Data.Bits
import Data.Word

interpretARM :: IsSystem s m => ARMInstruction -> m ()
interpretARM instr =
  case instr of
    Branch (Link l) offset -> do
      -- if the link bit is set, we put the current pc into r14
      when l $ do
        registers.r14 <~ use (registers.r15)
        registers.r14 -= 4
      -- this is maybe broken but we will never know
      registers.r15 %= \x -> fromIntegral (fromIntegral x + offset + 4)
    DataProcessing _ _ _ (RegisterName 15) (Left (RegisterShift _ _ _)) -> error "interpretARM: invalid r15 usage!"
    DataProcessing _ _ _ _ (Left (RegisterShift _ _ (RegisterName 15))) -> error "interpretARM: invalid r15 usage!"
    DataProcessing opcode (SetCondition setCond) dest@(RegisterName r) op1 op2 -> do
      (functionFromOpcode opcode) (registerLens dest) (registerLens op1) (operand2Lens op2) setCond
      when (r == 15) $ do
        val <- use $ registers.pc
        registers.pc .= val + 4
    HalfwordDataTransferRegister pp ud wb ls s g base dest offset ->
      handleHalfwordDataTransferRegister pp ud wb ls s g base dest offset
    HalfwordDataTransferImmediate pp ud wb ls s g base dest offset ->
      handleHalfWordDataTransferImmediate pp ud wb ls s g base dest offset
    SingleDataSwap g base dest src ->
      handleSingleDataSwap g base dest src
    SingleDataTransfer pp ud g wb ls dest src op2 ->
      handleSingleDataTransfer pp ud g wb ls dest src op2
    BlockDataTransfer pp ud user wb ls base rlist ->
      handleBlockDataTranfer pp ud user wb ls base rlist
    Multiply acc cond dest or0 or1 or2 ->
      handleMultiply acc cond dest or0 or1 or2
    MultiplyLong s acc cond destHi destLo or0 or1 ->
      handleMultiplyLong s acc cond destHi destLo or0 or1
    BranchExchange or0 ->
      handleBranchExchange or0
    CoprocessorDataTransfer -> error "Unimplemented instruction: Coprocessor data transfer"
    CoprocessorDataOperation -> error "Unimplemented instruction: Coprocessor data operation"
    CoprocessorRegisterTransfer -> error "Unimplemented instruction: Coprocessor register transfer"
    Undefined -> error "Interpreter received undefined instruction"
    SoftwareInterrupt -> error "Uninmplemented instruction: Software interrupt"
    --_ -> error "interpretARM: unknown instruction"

handleSingleDataTransfer :: IsSystem s m
                         => PrePost -> OffsetDirection -> (Granularity 'Full) -> WriteBack -> LoadStore -> RegisterName -> RegisterName -> Either (Shifted RegisterName) Offset -> m ()
handleSingleDataTransfer pp ud gran wb ls base target op2 = do
  baseVal <- use $ registers.rn base
  offsetVal <- either getShiftedRegister return op2
  let (addr, final) = mkPrePost pp ud baseVal offsetVal
  case (ls, gran) of
    (Load, Byte) -> do
      byte <- readAddressByte addr
      registers.rn target .= fromIntegral byte
    (Load, Word) -> do
      word <- readAddressWord $ addr .&. 0xFFFFFFFC
      registers.rn target .= word `rotateR` (fromIntegral $ $(bitmask 1 0) addr * 8)
    (Store, Byte) -> use (registers.rn target) >>= writeAddressByte addr . fromIntegral
    (Store, Word) -> use (registers.rn target) >>= writeAddressWord (addr .&. 0xFFFFFFFC)
  when wb $ registers.rn base .= final

handleBlockDataTranfer :: IsSystem s m => PrePost -> OffsetDirection -> ForceUserMode -> WriteBack -> LoadStore -> RegisterName -> RegisterList -> m ()
handleBlockDataTranfer pp ud user wb ls base rlist = do
  baseVal <- use (registers.rn base)
  let start = (directionToOperator ud) baseVal (if pp == Pre then 4 else 0)
  case (ls, user) of
    (_, True) -> error "Unimplemented feature: S bit set in BlockDataTransfer"
    (Load, False) -> do
      endVal <- readBlocks ud start rlist
      when wb $ registers.rn base .= endVal
    (Store, False) -> do
      endVal <- writeBlocks ud start rlist
      when wb $ registers.rn base .= endVal

readBlocks :: IsSystem s m => OffsetDirection -> MWord -> RegisterList -> m MWord
readBlocks _ w [] = return w
readBlocks d w (r:rs) = do
  (registers.rn r) <~ readAddressWord w
  readBlocks d (o w 4) rs
  where
    o = directionToOperator d

writeBlocks :: IsSystem s m => OffsetDirection -> MWord -> RegisterList -> m MWord
writeBlocks _ w [] = return w
writeBlocks d w (r:rs) = do
  val <- use (registers.rn r)
  writeAddressWord w val
  writeBlocks d (o w 4) rs
  where
    o = directionToOperator d

handleHalfwordDataTransferRegister :: IsSystem s m => PrePost -> OffsetDirection -> WriteBack -> LoadStore -> Signed -> (Granularity 'Lower) -> RegisterName -> RegisterName -> RegisterName -> m ()
handleHalfwordDataTransferRegister pp ud wb ls s g base dest offset = do
  bVal <- use (registers.rn base)
  oVal <- use (registers.rn offset)
  let (addr, final) = mkPrePost pp ud bVal oVal
  valHW <- readAddressHalfWord addr
  case (g, ls, s) of
    (Byte, Load, True) -> do
      let val = byteExtend $ fromIntegral $ $(bitmask 7 0) valHW
      registers.rn dest .= val
    (HalfWord, Load, True) -> do
      registers.rn dest .= halfWordExtend valHW
    (HalfWord, Load, False) -> do
      registers.rn dest .= fromIntegral valHW
    (HalfWord, Store, False) -> do
      destAddr <- use (registers.rn dest)
      writeAddressHalfWord destAddr valHW
    (_, _, _) -> error "handleHalfwordDataTransferRegister: Incorrect arguments passed to HalfWordDataTransfer instruction"
  when wb $ registers.rn base .= final

handleHalfWordDataTransferImmediate :: IsSystem s m => PrePost -> OffsetDirection -> WriteBack -> LoadStore -> Signed -> (Granularity 'Lower) -> RegisterName -> RegisterName -> Offset -> m ()
handleHalfWordDataTransferImmediate pp ud wb ls s g base dest offset = do
  bVal <- use (registers.rn base)
  let oVal = offset
  let readAddr = (directionToOperator ud) bVal (if pp == Pre then oVal else 0)
  valHW <- readAddressHalfWord readAddr
  (case (g, ls, s) of
    (Byte, Load, True) -> do
      let val = byteExtend $ fromIntegral $ $(bitmask 7 0) valHW
      registers.rn dest .= val
    (HalfWord, Load, True) -> do
      registers.rn dest .= halfWordExtend valHW
    (HalfWord, Load, False) -> do
      registers.rn dest .= fromIntegral valHW
    (HalfWord, Store, False) -> do
      destAddr <- use (registers.rn dest)
      writeAddressHalfWord destAddr valHW
    (_, _, _) -> error "handleHalfwordDataTransferImmediate: Incorrect arguments passed to HalfWordDataTransfer instruction")
  when (pp == Post) $
    registers.rn base .= (directionToOperator ud) bVal oVal
  when ((pp == Pre) && wb) $
    registers.rn base .= bVal

handleSingleDataSwap :: IsSystem s m => (Granularity 'Full) -> RegisterName -> RegisterName -> RegisterName -> m ()
handleSingleDataSwap g base dest src = case g of
  Byte -> do
    swapAddr <- use (registers.rn base)
    swapVal <- readAddressHalfWord swapAddr
    srcVal <- use (registers.rn src)
    let v1 = byteExtend $ (fromIntegral ($(bitmask 7 0) swapVal) :: Byte)
    let v2 = byteExtend $ (fromIntegral ($(bitmask 7 0) srcVal) :: Byte)
    writeAddressHalfWord swapAddr $ fromIntegral v2
    registers.rn dest .= fromIntegral v1
  Word -> do
    swapAddr <- use (registers.rn base)
    registers.rn dest <~ readAddressWord swapAddr
    srcVal <- use (registers.rn src)
    writeAddressWord swapAddr srcVal

handleMultiply :: IsSystem s m => Accumulate -> SetCondition -> RegisterName -> RegisterName -> RegisterName -> RegisterName -> m ()
handleMultiply acc (SetCondition cond) dest opReg0 opReg1 opReg2 = do
  val <- (*) <$> use (registers.rn opReg1) <*> use (registers.rn opReg2)
  case acc of
    False -> registers.rn dest .= val
    True -> do
      offset <- use (registers.rn opReg0)
      registers.rn dest .= (val + offset)
  when cond $ do
    endVal <- use (registers.rn dest)
    flags.negative .= testBit endVal 31
    flags.zero .= (endVal == 0)
    flags.carry .= False

handleMultiplyLong :: IsSystem s m => Signed -> Accumulate -> SetCondition -> RegisterName -> RegisterName -> RegisterName -> RegisterName -> m ()
handleMultiplyLong s acc (SetCondition cond) destHi destLo or0 or1 = do
  or0Val <- use (registers.rn or0)
  or1Val <- use (registers.rn or1)
  case s of
    True -> do
      let op0 = fromIntegral or0Val :: Integer
      let op1 = fromIntegral or1Val :: Integer
      case acc of
        False -> do
          let result = op0 * op1
          (registers.rn destHi) .= fromIntegral ((result .&. 0xFFFFFFFF00000000) `shiftR` 32)
          (registers.rn destLo) .= fromIntegral (result .&. 0x00000000FFFFFFFF)
        True -> do
          dhVal <- use (registers.rn destHi)
          dlVal <- use (registers.rn destLo)
          let op2 = ((fromIntegral $ dhVal :: Integer) `shiftL` 32) .&. (fromIntegral dlVal :: Integer)
          let result = op0 * op1 + op2
          (registers.rn destHi) .= fromIntegral ((result .&. 0xFFFFFFFF00000000) `shiftR` 32)
          (registers.rn destLo) .= fromIntegral (result .&. 0x00000000FFFFFFFF)
    False -> do
      let op0 = fromIntegral or0Val :: Word64
      let op1 = fromIntegral or1Val :: Word64
      case acc of
        False -> do
          let result = op0 * op1
          (registers.rn destHi) .= fromIntegral ((result .&. 0xFFFFFFFF00000000) `shiftR` 32)
          (registers.rn destLo) .= fromIntegral (result .&. 0x00000000FFFFFFFF)
        True -> do
          dhVal <- use (registers.rn destHi)
          dlVal <- use (registers.rn destLo)
          let op2 = ((fromIntegral $ dhVal :: Word64) `shiftL` 32) .&. (fromIntegral dlVal :: Word64)
          let result = op0 * op1 + op2
          (registers.rn destHi) .= fromIntegral ((result .&. 0xFFFFFFFF00000000) `shiftR` 32)
          (registers.rn destLo) .= fromIntegral (result .&. 0x00000000FFFFFFFF)
  when cond $ do
    val <- use (registers.rn destHi)
    valLower <- use (registers.rn destLo)
    flags.negative .= val `testBit` 31
    flags.zero .= ((val + valLower) == 0)
    flags.carry .= False
    flags.overflow .= False

handleBranchExchange :: IsSystem s m => RegisterName -> m ()
handleBranchExchange opReg = do
  op' <- use (registers.rn opReg)
  let thumb = op' `testBit` 0
  let realOp = op' .&. 0xFFFFFFFE
  registers.pc .= (realOp + 4)
  flags.thumbStateBit .= thumb

getShiftedRegister :: IsSystem s m => Shifted RegisterName -> m MWord
getShiftedRegister (AmountShift n st r) = do
  (isCarry, val) <- applyShiftTypeWithCarry st <$> use (registers.rn r)
                                               <*> pure (fromIntegral (fromIntegral n :: HalfWord))
                                               <*> use (flags.carry)
  flags.carry .= isCarry
  return val
getShiftedRegister (RegisterShift n st r) = do
  (isCarry, val) <- applyShiftTypeWithCarry st <$> use (registers.rn r)
                                               <*> (getLSB <$> use (registers.rn n))
                                               <*> use (flags.carry)
  flags.carry .= isCarry
  return val
  where getLSB x = fromIntegral (fromIntegral (fromIntegral x :: Word8) :: Word16) :: Int

directionToOperator :: Num a => OffsetDirection -> (a -> a -> a)
directionToOperator d = case d of
  Up -> (+)
  Down -> (-)

mkPrePost :: Num t => PrePost -> OffsetDirection -> t -> t -> (t, t)
mkPrePost Pre ud base offset =
  (directionToOperator ud base offset, directionToOperator ud base offset)
mkPrePost Post ud base offset =
  (base, directionToOperator ud base offset)
