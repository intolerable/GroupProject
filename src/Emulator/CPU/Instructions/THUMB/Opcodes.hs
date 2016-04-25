module Emulator.CPU.Instructions.THUMB.Opcodes where

import Emulator.CPU hiding (CPUMode(..), Interrupt(..))
import Emulator.CPU.Instructions.THUMB
import Emulator.CPU.Instructions.Types
import Emulator.Interpreter.Monad
import Emulator.Types

import Data.Bits

and :: Monad m => RegisterName -> RegisterName -> SystemT m ()
and src dest = undefined
