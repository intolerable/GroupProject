module Emulator.Args
  ( Args(..)
  , getArgs ) where

import Options.Applicative

data Args =
  Args { headless :: Bool
       , slowMode :: Bool
       , debugMode :: Bool
       , verbose :: Bool
       , biosFile :: FilePath
       , romFile :: FilePath }
  deriving (Show, Read, Eq, Ord)

args :: Parser Args
args = Args <$> switch (long "headless" <> help "whether to run the emulator with display disabled")
            <*> switch (long "slow" <> help "whether to run the emulator with slow mode enabled")
            <*> switch (long "debug" <> help "whether to run the emulator with debug mode enabled")
            <*> switch (long "verbose" <> help "whether to run the emulator with verbose mode enabled")
            <*> strArgument (metavar "BIOS_FILE")
            <*> strArgument (metavar "ROM_FILE")

getArgs :: IO Args
getArgs = execParser opts
  where
    opts = info (helper <*> args) fullDesc
