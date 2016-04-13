module Emulator.Args where

import Options.Applicative

data Args =
  Args { headless :: Bool
       , biosFileLocation :: FilePath }
  deriving (Show, Read, Eq, Ord)

args :: Parser Args
args = Args <$> switch (long "headless" <> help "whether to run the emulator with display disabled")
            <*> pure "./res/gba_bios.bin"

getArgs :: IO Args
getArgs = execParser opts
  where
    opts = info (helper <*> args) fullDesc
