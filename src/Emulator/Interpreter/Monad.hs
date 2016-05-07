module Emulator.Interpreter.Monad where

import Emulator.CPU
import Emulator.Debug
import Emulator.Memory
import Emulator.Memory.AddressSpace
import Emulator.Types

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array.IArray
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class
import qualified Control.Monad.State.Class as State
import qualified Data.ByteString.Lazy as BS

data SystemState =
  SystemState { _systemStateSysRegisters :: Registers
              , _systemStateSysBIOS :: Memory
              , _systemStateSysROM :: Memory
              , _systemStateSysGamePakWRAM :: Memory
              , _systemStateSysIORegisters :: Memory
              , _systemStateSysPaletteRAM :: Memory
              , _systemStateSysRAM :: Memory
              , _systemStateSysOAM :: Memory
              , _systemStateSysVRAM :: Memory }
  deriving (Show, Eq)

makeFields ''SystemState

buildInitialState :: ByteString -> ByteString -> SystemState
buildInitialState rom bios =
  SystemState (def & r15 .~ 0x00000004) biosArray romArray initialGamePakWRAM initialIORegs initialPaletteRAM initialRAM initialOAM initialVRAM
    where
      initialRAM = accumArray const 0 (0x02000000, 0x0203FFFF) []
      initialVRAM = accumArray const 0 (0x06000000, 0x06017FFF) []
      initialGamePakWRAM = accumArray const 0 (0x03000000, 0x03007FFF) []
      initialOAM = accumArray const 0 (0x07000000, 0x070003FF) []
      initialIORegs = accumArray const 0 (0x04000000, 0x040003FF) []
      initialPaletteRAM = accumArray const 0 (0x05000000, 0x050003FF) []
      -- not totally sure that this is producing the correct output
      romArray = accumArray (flip const) 0 (0x08000000, 0x0DFFFFFF) $ zip [0x08000000..] $ BS.unpack rom
      biosArray = accumArray (flip const) 0 (0x00000000, 0x00003FFF) $ zip [0x00000000..] $ BS.unpack bios

instance HasRegisters SystemState where
  registers = sysRegisters

instance HasFlags SystemState where
  flags = sysRegisters.flags

newtype SystemT m a =
  SystemT { unSystem :: StateT SystemState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

runSystemT :: SystemT m a -> SystemState -> m (a, SystemState)
runSystemT (SystemT a) x = runStateT a x

instance MonadIO m => Debug (SystemT m) where
  debug lvl str = liftIO $ debug lvl str

instance Monad m => CanWrite WRAM (SystemT m) where
  writeByte _ a b = SystemT $ zoom sysRAM $ modify (// [(a, b)])

instance Monad m => CanRead WRAM (SystemT m) where
  readByte _ a = SystemT $ zoom sysRAM $ gets (! a)

instance Monad m => CanWrite GamePakWRAM (SystemT m) where
  writeByte _ a b = SystemT $ zoom sysGamePakWRAM $ modify (// [(a, b)])

instance Monad m => CanRead GamePakWRAM (SystemT m) where
  readByte _ a = SystemT $ zoom sysGamePakWRAM $ gets (! a)

instance Monad m => CanWrite IORegisters (SystemT m) where
  writeByte _ a b = SystemT $ zoom sysIORegisters $ modify (// [(a, b)])

instance Monad m => CanRead IORegisters (SystemT m) where
  readByte _ a = SystemT $ zoom sysIORegisters $ gets (! a)

instance Monad m => CanWrite PaletteRAM (SystemT m) where
  writeByte _ a b = SystemT $ zoom sysPaletteRAM $ modify (// [(a, b)])

instance Monad m => CanRead PaletteRAM (SystemT m) where
  readByte _ a = SystemT $ zoom sysPaletteRAM $ gets (! a)

instance Monad m => CanRead ROM (SystemT m) where
  readByte _ a = SystemT $ zoom sysROM $ gets (! a)

instance Monad m => CanRead BIOS (SystemT m) where
  readByte _ a = SystemT $ zoom sysBIOS $ gets (! a)

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

type IsSystem s m = (AddressSpace m, State.MonadState s m, HasRegisters s, HasFlags s)
