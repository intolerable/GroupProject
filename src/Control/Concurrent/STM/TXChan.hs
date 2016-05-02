module Control.Concurrent.STM.TXChan
  ( TXChan()
  , newTXChan
  , newTXChanIO
  , newEmptyTXChan
  , newEmptyTXChanIO
  , writeTXChan
  , readTXChan
  , takeTXChan ) where

import Control.Concurrent.STM

data TXChan a = TXChan (TVar (Maybe a))
  deriving (Eq)

newTXChan :: a -> STM (TXChan a)
newTXChan x = TXChan <$> newTVar (Just x)

newTXChanIO :: a -> IO (TXChan a)
newTXChanIO x = TXChan <$> newTVarIO (Just x)

newEmptyTXChan :: STM (TXChan a)
newEmptyTXChan = TXChan <$> newTVar Nothing

newEmptyTXChanIO :: IO (TXChan a)
newEmptyTXChanIO = TXChan <$> newTVarIO Nothing

writeTXChan :: TXChan a -> a -> STM ()
writeTXChan (TXChan v) a = writeTVar v (Just a)

readTXChan :: TXChan a -> STM (Maybe a)
readTXChan (TXChan v) = readTVar v

takeTXChan :: TXChan a -> STM a
takeTXChan (TXChan v) = do
  val <- readTVar v
  case val of
    Just x -> return x
    Nothing -> retry
