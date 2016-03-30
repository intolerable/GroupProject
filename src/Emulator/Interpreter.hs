module Emulator.Interpreter where

import Emulator.CPU hiding (System)
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

newtype System m a =
  System { runSystem :: StateT SystemState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance Monad m => CanWrite WRAM (System m) where
  writeByte _ a b = System $ zoom sysRAM $ modify (// [(a, b)])

instance Monad m => CanRead WRAM (System m) where
  readByte _ a = System $ zoom sysRAM $ gets (! a)

instance Monad m => CanRead ROM (System m) where
  readByte _ a = System $ zoom sysROM $ gets (! a)

instance Monad m => CanWrite VRAM (System m) where
  writeByte _ a b = System $ zoom sysVRAM $ modify (// [(a, b)])

instance Monad m => CanRead VRAM (System m) where
  readByte _ a = System $ zoom sysVRAM $ gets (! a)

instance Monad m => CanWrite OAM (System m) where
  writeByte _ a b = System $ zoom sysOAM $ modify (// [(a, b)])

instance Monad m => CanRead OAM (System m) where
  readByte _ a = System $ zoom sysOAM $ gets (! a)

instance Monad m => State.MonadState SystemState (System m) where
  state = System . State.state

interpretLoop :: MonadIO m => System m ()
interpretLoop = go False
  where
    go inc = do
      when inc $ sysRegisters.r15 += 4
      pc <- System (use (sysRegisters.r15))
      liftIO $ putStrLn $ showHex pc
      newInstr <- System (use (sysRegisters.r15)) >>= readAddressWord
      liftIO $ putStrLn $ showHex newInstr
      case parseARM newInstr of
        Left err -> error $ "interpretLoop: instruction parse failed (" ++ err ++ ")"
        Right (cond, instr) -> do
          liftIO $ print (cond, instr)
          conditionally cond $ interpretARM instr
          go True

interpretARM :: Monad m => Instruction ARM -> System m ()
interpretARM instr =
  case instr of
    Branch (Link l) offset -> do
      -- if the link bit is set, we put the current pc into r14
      when l $ System $ sysRegisters.r14 <~ use (sysRegisters.r15)
      -- not totally sure how prefetch interacts with this, so we'll just assume we're an instruction ahead at the moment
      System $ sysRegisters.r15 %= \x -> fromIntegral (fromIntegral x + offset + 4)
    DataProcessing opcode (SetCondition setCond) dest op1 op2 -> do
      System $ (functionFromOpcode opcode) (registerLens dest) (registerLens op1) (operand2Lens op2) setCond
    SingleDataTransfer pp ud g wb ls dest src op2 ->
      handleSingleDataTransfer pp ud g wb ls dest src op2
    _ -> error "interpretARM: unknown instruction"

handleSingleDataTransfer :: Monad m
                         => PrePost -> OffsetDirection -> Granularity -> WriteBack -> LoadStore -> RegisterName -> RegisterName -> Either (Shifted RegisterName) Offset -> System m ()
handleSingleDataTransfer _pp _ud _g _wb _ls _dest _src _op2 =
  error "handleSingleDataTransfer: unimplemented instruction"
