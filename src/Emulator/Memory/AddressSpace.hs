module Emulator.Memory.AddressSpace where

import Emulator.Types

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Array.IArray
import Data.Array.MArray
import Data.Bits
import Data.Proxy

class (Monad m) => CanWrite space m where
  writeByte :: Proxy space -> Address -> Byte -> m ()

  writeHalfWord :: Proxy space -> Address -> HalfWord -> m ()
  writeHalfWord p addr hw = do
    writeByte p addr (fromIntegral hw)
    writeByte p (succ addr) (fromIntegral (hw `shiftR` 8))

  writeWord :: Proxy space -> Address -> MWord -> m ()
  writeWord p addr w = do
    writeByte p addr (fromIntegral w)
    writeByte p (addr + 1) (fromIntegral (w `shiftR` 8))
    writeByte p (addr + 2) (fromIntegral (w `shiftR` 16))
    writeByte p (addr + 3) (fromIntegral (w `shiftR` 24))

class (Monad m) => CanRead space m where
  readByte :: Proxy space -> Address -> m Byte

  readHalfWord :: Proxy space -> Address -> m HalfWord
  readHalfWord p addr = do
    b1 <- fromIntegral <$> readByte p addr
    b2 <- fromIntegral <$> readByte p (succ addr)
    return $ b1 .|. (b2 `shiftL` 8)

  readWord :: Proxy space -> Address -> m MWord
  readWord p addr = do
    b1 <- fromIntegral <$> readByte p addr
    b2 <- fromIntegral <$> readByte p (addr + 1)
    b3 <- fromIntegral <$> readByte p (addr + 2)
    b4 <- fromIntegral <$> readByte p (addr + 3)
    return $
      b1 .|. (b2 `shiftL` 8)
         .|. (b3 `shiftL` 16)
         .|. (b4 `shiftL` 24)

data OAM
data ROM
data VRAM
data WRAM

newtype PureMemoryT x m a = PureMemoryT (StateT Memory m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance Monad m => CanRead x (PureMemoryT x m) where
  readByte _ a =
    PureMemoryT (gets (! a))

instance Monad m => CanWrite x (PureMemoryT x m) where
  writeByte _ a b =
    PureMemoryT (modify (// [(a, b)]))

newtype MutableMemoryT x m a =
  MutableMemoryT { runMutableMemoryT :: ReaderT MemoryIO m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance MonadIO m => CanWrite x (MutableMemoryT x m) where
  writeByte _ a b = do
    arr <- MutableMemoryT ask
    liftIO $ writeArray arr a b

instance MonadIO m => CanRead x (MutableMemoryT x m) where
  readByte _ a = do
    arr <- MutableMemoryT ask
    liftIO $ readArray arr a
