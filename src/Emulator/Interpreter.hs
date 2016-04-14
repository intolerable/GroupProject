module Emulator.Interpreter where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.Debug
import Emulator.Interpreter.Monad
import Emulator.Interpreter.ARM
import Emulator.Memory
import Emulator.Types
import Utilities.Show

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Text.Format

interpretLoop :: MonadIO m => SystemT m ()
interpretLoop = go True
  where
    go inc = do
      when inc $ sysRegisters.r15 += 4
      pc <- prefetched <$> use (sysRegisters.r15) -- adjusted for prefetch
      newInstr <- readAddressWord pc
      case parseARM newInstr of
        Left err -> do
          debug Error $ format "{} {} {}" (showHex pc, showHex newInstr, show err)
          error $ "interpretLoop: instruction parse failed (" ++ err ++ ")"
        Right (cond, instr) -> do
          debug Info $ format "pc: {}, instr: {}, condition: {}\n        {}"
            (showHex pc, showHex newInstr, show cond, show instr)
          conditionally cond $ interpretARM instr
          go True

prefetched :: Address -> Address
prefetched addr = addr - 4
