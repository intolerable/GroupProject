module Emulator.Memory where

import Emulator.Types

import Control.Monad.Trans.State.Strict
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Bits
import Prelude hiding (Word)

type Memory = UArray Address Byte
type MemoryIO = IOUArray Address Byte

class Mem m where

  writeByte :: Address -> Byte -> m ()
  readByte :: Address -> m Byte

  -- assume little endian
  writeWord :: Address -> Word -> m ()
  writeWord addr word = do
    writeByte addr (fromIntegral word)
    writeByte (addr + 1) (fromIntegral word `shiftR` 8)
    writeByte (addr + 2) (fromIntegral word `shiftR` 16)
    writeByte (addr + 3) (fromIntegral word `shiftR` 24)
  readWord :: Address -> m Word
  readWord addr = do
    b1 <- fromIntegral <$> readByte addr
    b2 <- fromIntegral <$> readByte (addr + 1)
    b3 <- fromIntegral <$> readByte (addr + 2)
    b4 <- fromIntegral <$> readByte (addr + 3)
    b1 .|. (b2 `shiftL` 8) .|. (b3 `shiftL` 16) .|. (b4 `shiftL` 24)

newtype PureMemory a = PureMemory (State Memory a)
  deriving (Functor, Applicative, Monad)

instance Mem PureMemory where
  writeByte a b =
    PureMemory (modify (// [(a, b)]))
  readByte a =
    PureMemory (gets (! a))

newtype MutableMemory a = MutableMemory { runMutableMemory :: MemoryIO -> IO a }
  deriving (Functor)

instance Applicative MutableMemory where
  pure = MutableMemory . pure . pure
  MutableMemory f <*> MutableMemory g =
    MutableMemory (\x -> f x <*> g x)

instance Monad MutableMemory where
  return = pure
  MutableMemory a >>= f =
    MutableMemory $ \x -> do
      res <- a x
      runMutableMemory (f res) x

instance Mem MutableMemory where
  writeByte a b =
    MutableMemory (\arr -> writeArray arr a b)
  readByte a =
    MutableMemory (\arr -> readArray arr a)
