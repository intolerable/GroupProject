module Emulator.Memory.RAM where

import Emulator.Memory.Region
import Emulator.Types

import Control.Monad.Trans.State.Strict
import Data.Array.IO
import Data.Array.Unboxed
import Data.Bits

data WRAM

newtype PureMemory a = PureMemory { runPureMemory :: State Memory a }
  deriving (Functor, Applicative, Monad)

instance CanWrite WRAM PureMemory where
  writeByte _ a b =
    PureMemory (modify (// [(a, b)]))

instance CanRead WRAM PureMemory where
  readByte _ a =
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

instance CanWrite WRAM MutableMemory where
  writeByte _ a b =
    MutableMemory (\arr -> writeArray arr a b)

instance CanRead WRAM MutableMemory where
  readByte _ a =
    MutableMemory (\arr -> readArray arr a)
