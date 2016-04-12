module Emulator.Debug where

import Data.Text.Lazy (Text)
import Data.Text.Format
import qualified Data.Text.Lazy.IO as Text

data DebugLevel = Info
                | Warning
                | Error
  deriving (Show, Read, Eq)

class Monad m => Debug m where
  debug :: DebugLevel -> Text -> m ()

instance Debug IO where
  debug lvl str = Text.putStrLn $ format "[{}]: {}" (show lvl, str)
