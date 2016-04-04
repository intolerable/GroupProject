module Emulator.Interpreter where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.CPU.Instructions.Parser
import Emulator.Memory
import Emulator.Memory.AddressSpace
import Emulator.Types
import Utilities.Show

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
interpretLoop = go False
  where
    go inc = do
      when inc $ sysRegisters.r15 += 4
      pc <- SystemT (use (sysRegisters.r15))
      liftIO $ putStrLn $ showHex pc
      newInstr <- SystemT (use (sysRegisters.r15)) >>= readAddressWord
      liftIO $ putStrLn $ showHex newInstr
      case parseARM newInstr of
        Left err -> error $ "interpretLoop: instruction parse failed (" ++ err ++ ")"
        Right (cond, instr) -> do
          liftIO $ print (cond, instr)
          conditionally cond $ interpretARM instr
          go True

interpretARM :: Monad m => Instruction ARM -> SystemT m ()
interpretARM instr =
  case instr of
    Branch (Link l) offset -> do
      -- if the link bit is set, we put the current pc into r14
      when l $ SystemT $ sysRegisters.r14 <~ use (sysRegisters.r15)
      -- not totally sure how prefetch interacts with this, so we'll just assume we're an instruction ahead at the moment
      SystemT $ sysRegisters.r15 %= \x -> fromIntegral (fromIntegral x + offset + 4)
    DataProcessing opcode (SetCondition setCond) dest op1 op2 -> do
      SystemT $ (functionFromOpcode opcode) (registerLens dest) (registerLens op1) (operand2Lens op2) setCond
    SingleDataTransfer pp ud g wb ls dest src op2 ->
      handleSingleDataTransfer pp ud g wb ls dest src op2
    _ -> error "interpretARM: unknown instruction"

handleSingleDataTransfer :: Monad m
                         => PrePost -> OffsetDirection -> Granularity -> WriteBack -> LoadStore -> RegisterName -> RegisterName -> Either (Shifted RegisterName) Offset -> SystemT m ()
handleSingleDataTransfer _pp _ud _g _wb _ls _dest _src _op2 =
  error "handleSingleDataTransfer: unimplemented instruction"
