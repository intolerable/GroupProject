module Emulator.Memory where

import Emulator.Types

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Array.IO
import Data.Array.Unboxed
import Data.Bits
import Prelude

type Memory = UArray Address Byte
type MemoryIO = IOUArray Address Byte

class (Functor m, Monad m) => Mem m where

  writeByte :: Address -> Byte -> m ()
  readByte :: Address -> m Byte

  writeWordLE :: Address -> MWord -> m ()
  writeWordLE addr word = do
    writeByte addr (fromIntegral word)
    writeByte (addr + 1) (fromIntegral word `shiftR` 8)
    writeByte (addr + 2) (fromIntegral word `shiftR` 16)
    writeByte (addr + 3) (fromIntegral word `shiftR` 24)
  readWordLE :: Address -> m MWord
  readWordLE addr = do
    b1 <- fromIntegral <$> readByte addr
    b2 <- fromIntegral <$> readByte (addr + 1)
    b3 <- fromIntegral <$> readByte (addr + 2)
    b4 <- fromIntegral <$> readByte (addr + 3)
    return $ b1 .|. (b2 `shiftL` 8) .|. (b3 `shiftL` 16) .|. (b4 `shiftL` 24)

newtype PureMemory a = PureMemory { runPureMemory :: State Memory a }
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

type AddressSpace m = (Functor m, Monad m, Mem m)

writeAddress :: AddressSpace m => Address -> Byte -> m ()
writeAddress = undefined

readAddress :: AddressSpace m => Address -> m Byte
readAddress = undefined
