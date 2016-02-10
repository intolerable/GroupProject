module Emulator.Memory where

import Emulator.Types

import Control.Monad.Trans.State.Strict
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed
import Prelude hiding (Word)

type Memory = UArray Address Byte
type MemoryIO = IOUArray Address Byte

class Mem m where

  writeByte :: Address -> Byte -> m ()
  readByte :: Address -> m Byte

  writeWord :: Address -> Word -> m ()
  writeWord = undefined -- based on multiple writeBytes
  readWord :: Address -> m Word
  readWord = undefined -- based on multiple readBytes

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
