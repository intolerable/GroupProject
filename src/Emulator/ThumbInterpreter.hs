module Emulator.ThumbInterpreter where

import Emulator.CPU.Instructions
import Emulator.Interpreter (SystemT)

interpretThumb :: Monad m => THUMBInstruction -> SystemT m ()
interpretThumb = undefined
