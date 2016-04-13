module Emulator.Args
  ( Args(..)
  , getArgs ) where

import Options.Applicative

data Args =
  Args { headless :: Bool
       , biosFile :: FilePath
       , romFile :: FilePath }
  deriving (Show, Read, Eq, Ord)

args :: Parser Args
args = Args <$> switch (long "headless" <> help "whether to run the emulator with display disabled")
            <*> strArgument (metavar "BIOS_FILE")
            <*> strArgument (metavar "ROM_FILE")

getArgs :: IO Args
getArgs = execParser opts
  where
    opts = info (helper <*> args) fullDesc
