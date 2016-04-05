module Emulator.Interpreter where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.CPU.Instructions.Parser
import Emulator.Memory
import Emulator.Memory.AddressSpace
import Emulator.Types
import Utilities.Show
import Utilities.Parser.TemplateHaskell

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array.Unboxed
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class
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

interpretARM :: Monad m => Instruction ARM -> SystemT m ()
interpretARM instr =
  case instr of
    Branch (Link l) offset -> SystemT $ do
      -- if the link bit is set, we put the current pc into r14
      when l $ sysRegisters.r14 <~ use (sysRegisters.r15)
      sysRegisters.r15 %= \x -> fromIntegral (fromIntegral x + offset)
    DataProcessing opcode (SetCondition setCond) dest op1 op2 -> do
      (functionFromOpcode opcode) (registerLens dest) (registerLens op1) (operand2Lens op2) setCond
    SingleDataSwap g base dest src ->
      handleSingleDataSwap g base dest src
    SingleDataTransfer pp ud g wb ls dest src op2 ->
      handleSingleDataTransfer pp ud g wb ls dest src op2
    BlockDataTransfer pp ud user wb ls base rlist ->
      handleBlockDataTranfer pp ud user wb ls base rlist
    _ -> error "interpretARM: unknown instruction"

handleSingleDataTransfer :: Monad m
                         => PrePost -> OffsetDirection -> Granularity -> WriteBack -> LoadStore -> RegisterName -> RegisterName -> Either (Shifted RegisterName) Offset -> SystemT m ()

handleSingleDataTransfer _pp ud gran _wb ls base _target op2 = do
  _addr <- offsetDir <$> use (registers.registerLens base) <*> use (registers.targetLens)
  case (ls, gran) of
    (Load, Byte) -> undefined
    (Load, Word) -> undefined
    (Store, Byte) -> undefined
    (Store, Word) -> undefined
  where
    targetLens = case op2 of { Left x -> shiftedRegisterLens x; Right x -> to (const x) }
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


handleSingleDataSwap :: Monad m => Granularity -> RegisterName -> RegisterName -> RegisterName -> SystemT m ()
handleSingleDataSwap g base dest src = case g of
  Byte -> do
    swapAddr <- use (registers.rn base)
    swapVal <- readAddressHalfWord swapAddr
    srcVal <- use (registers.rn src)
    let v1 = $(bitmask 7 0) swapVal -- FIXME: Sign Extension
    let v2 = $(bitmask 7 0) srcVal
    writeAddressHalfWord swapAddr $ fromIntegral v2
    registers.rn dest .= fromIntegral v1
  Word -> do
    swapAddr <- use (registers.rn base)
    registers.rn dest <~ readAddressWord swapAddr
    srcVal <- use (registers.rn src)
    writeAddressWord swapAddr srcVal
  _ -> error "Incorrect granularity supplied for SingleDataSwap"


directionToOperator :: Num a => OffsetDirection -> (a -> a -> a)
directionToOperator d = case d of
  Up -> (+)
  Down -> (-)
