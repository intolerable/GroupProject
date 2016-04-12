module Emulator.Interpreter where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.Memory
import Emulator.Memory.AddressSpace
import Emulator.Types
import Utilities.Bits
import Utilities.Show
import Utilities.Parser.TemplateHaskell

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array.Unboxed
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class
import Data.Word
import qualified Control.Monad.State.Class as State
import qualified Data.ByteString.Lazy as BS

data SystemState =
  SystemState { _systemStateSysRegisters :: Registers
              , _systemStateSysROM :: Memory
              , _systemStateSysRAM :: Memory
              , _systemStateSysOAM :: Memory
              , _systemStateSysVRAM :: Memory }
  deriving (Show, Eq)

makeFields ''SystemState

buildInitialState :: ByteString -> SystemState
buildInitialState bs =
  SystemState (def & r15 .~ 0x08000000) romArray initialRAM initialVRAM initialOAM
    where
      initialRAM = accumArray const 0 (0x02000000, 0x0203FFFF) []
      initialVRAM = accumArray const 0 (0x06000000, 0x06017FFF) []
      initialOAM = accumArray const 0 (0x07000000, 0x070003FF) []
      -- not totally sure that this is producing the correct output
      romArray = accumArray (flip const) 0 (0x08000000, 0x0DFFFFFF) $ zip [0x8000000..] $ BS.unpack bs

instance HasRegisters SystemState where
  registers = sysRegisters

instance HasFlags SystemState where
  flags = sysRegisters.flags

newtype SystemT m a =
  SystemT { unSystem :: StateT SystemState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

runSystemT :: SystemT m a -> SystemState -> m (a, SystemState)
runSystemT (SystemT a) x = runStateT a x

instance Monad m => CanWrite WRAM (SystemT m) where
  writeByte _ a b = SystemT $ zoom sysRAM $ modify (// [(a, b)])

instance Monad m => CanRead WRAM (SystemT m) where
  readByte _ a = SystemT $ zoom sysRAM $ gets (! a)

instance Monad m => CanRead ROM (SystemT m) where
  readByte _ a = SystemT $ zoom sysROM $ gets (! a)

instance Monad m => CanWrite VRAM (SystemT m) where
  writeByte _ a b = SystemT $ zoom sysVRAM $ modify (// [(a, b)])

instance Monad m => CanRead VRAM (SystemT m) where
  readByte _ a = SystemT $ zoom sysVRAM $ gets (! a)

instance Monad m => CanWrite OAM (SystemT m) where
  writeByte _ a b = SystemT $ zoom sysOAM $ modify (// [(a, b)])

instance Monad m => CanRead OAM (SystemT m) where
  readByte _ a = SystemT $ zoom sysOAM $ gets (! a)

instance Monad m => State.MonadState SystemState (SystemT m) where
  state = SystemT . State.state

interpretLoop :: MonadIO m => SystemT m ()
interpretLoop = go True
  where
    go inc = do
      when inc $ sysRegisters.r15 += 4
      pc <- prefetched <$> use (sysRegisters.r15) -- adjusted for prefetch
      liftIO $ putStrLn $ showHex pc
      newInstr <- readAddressWord pc
      liftIO $ putStrLn $ showHex newInstr
      case parseARM newInstr of
        Left err -> error $ "interpretLoop: instruction parse failed (" ++ err ++ ")"
        Right (cond, instr) -> do
          liftIO $ print (cond, instr)
          conditionally cond $ interpretARM instr
          go True

prefetched :: Address -> Address
prefetched addr = addr - 4

interpretARM :: Monad m => ARMInstruction -> SystemT m ()
interpretARM instr =
  case instr of
    Branch (Link l) offset -> SystemT $ do
      -- if the link bit is set, we put the current pc into r14
      when l $ sysRegisters.r14 <~ use (sysRegisters.r15)
      sysRegisters.r15 %= \x -> fromIntegral (fromIntegral x + offset)
    DataProcessing opcode (SetCondition setCond) dest op1 op2 -> do
      (functionFromOpcode opcode) (registerLens dest) (registerLens op1) (operand2Lens op2) setCond
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
    Emulator.CPU.Instructions.Undefined -> error "Interpreter received undefined instruction"
    Emulator.CPU.Instructions.SoftwareInterrupt -> error "Uninmplemented instruction: Software interrupt"
    --_ -> error "interpretARM: unknown instruction"

handleSingleDataTransfer :: Monad m
                         => PrePost -> OffsetDirection -> (Granularity 'Full) -> WriteBack -> LoadStore -> RegisterName -> RegisterName -> Either (Shifted RegisterName) Offset -> SystemT m ()
handleSingleDataTransfer _pp ud gran _wb ls base target op2 = do
  addr <- offsetDir <$> use (registers.rn base) <*> use (registers.offsetLens)
  case (ls, gran) of
    (Load, Byte) -> error "sdt load byte"
    (Load, Word) -> error "sdt load word"
    (Store, Byte) -> error "sdt store byte"
    (Store, Word) -> use (registers.rn target) >>= writeAddressWord ($(bitmask 31 2) addr)
  where
    offsetLens = case op2 of { Left x -> shiftedRegisterLens x; Right x -> to (const x) }
    offsetDir = case ud of { Up -> (+); Down -> (-) }

handleBlockDataTranfer :: Monad m => PrePost -> OffsetDirection -> ForceUserMode -> WriteBack -> LoadStore -> RegisterName -> RegisterList -> SystemT m ()
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

readBlocks :: Monad m => OffsetDirection -> MWord -> RegisterList -> SystemT m MWord
readBlocks _ w [] = return w
readBlocks d w (r:rs) = do
  (registers.rn r) <~ readAddressWord w
  readBlocks d (o w 4) rs
  where
    o = directionToOperator d

writeBlocks :: Monad m => OffsetDirection -> MWord -> RegisterList -> SystemT m MWord
writeBlocks _ w [] = return w
writeBlocks d w (r:rs) = do
  val <- use (registers.rn r)
  writeAddressWord w val
  writeBlocks d (o w 4) rs
  where
    o = directionToOperator d

handleHalfwordDataTransferRegister :: Monad m => PrePost -> OffsetDirection -> WriteBack -> LoadStore -> Signed -> (Granularity 'Lower) -> RegisterName -> RegisterName -> RegisterName -> SystemT m ()
handleHalfwordDataTransferRegister pp ud wb ls s g base dest offset = do
  bVal <- use (registers.rn base)
  oVal <- use (registers.rn offset)
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
    (_, _, _) -> error "handleHalfwordDataTransferRegister: Incorrect arguments passed to HalfWordDataTransfer instruction")
  when (pp == Post) $
        registers.rn base .= (directionToOperator ud) bVal oVal
  when ((pp == Pre) && wb) $
        registers.rn base .= bVal

handleHalfWordDataTransferImmediate :: Monad m => PrePost -> OffsetDirection -> WriteBack -> LoadStore -> Signed -> (Granularity 'Lower) -> RegisterName -> RegisterName -> Offset -> SystemT m ()
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



handleSingleDataSwap :: Monad m => (Granularity 'Full) -> RegisterName -> RegisterName -> RegisterName -> SystemT m ()
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

handleMultiply :: Monad m => Accumulate -> SetCondition -> RegisterName -> RegisterName -> RegisterName -> RegisterName -> SystemT m ()
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

handleMultiplyLong :: Monad m => Signed -> Accumulate -> SetCondition -> RegisterName -> RegisterName -> RegisterName -> RegisterName -> SystemT m ()
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

handleBranchExchange :: Monad m => RegisterName -> SystemT m ()
handleBranchExchange opReg = do
  op' <- use (registers.rn opReg)
  let thumb = op' `testBit` 0 
  let realOp = op' .&. 0xFFFFFFFE
  registers.r15 .= realOp
  flags.thumbStateBit .= thumb

directionToOperator :: Num a => OffsetDirection -> (a -> a -> a)
directionToOperator d = case d of
  Up -> (+)
  Down -> (-)
