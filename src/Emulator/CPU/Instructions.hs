module Emulator.CPU.Instructions
  ( runCondition
  , conditionally
  , module Export ) where

import Emulator.CPU
import Emulator.CPU.Instructions.Types as Export
import Emulator.CPU.Instructions.ARM.Opcodes as Export (functionFromOpcode)
import Emulator.CPU.Instructions.ARM as Export
import Emulator.CPU.Instructions.THUMB as Export
import Emulator.CPU.Instructions.ARM.Parser as Export (parseARM)
import Emulator.CPU.Instructions.THUMB.Parser as Export (parseTHUMB)

import Control.Monad
import Control.Lens
import Control.Monad.State.Class
import Prelude hiding (Ordering(..))

-- | @conditionally cond act@ runs @act@ if the current state registers match up with
--     the given condition.
conditionally :: (HasFlags s, MonadState s m) => Condition -> m a -> m ()
conditionally cond act = do
  res <- runCondition cond
  when res $ void act

-- | Check if the current state registers match up with the given condition - if the
--     instruction would be executed, returns @True@, and if it wouldn't, returns @False@.
runCondition :: (HasFlags s, MonadState s m) => Condition -> m Bool
runCondition cond =
  case cond of
    EQ -> use (flags.zero)
    NE -> not <$> use (flags.zero)
    CS -> use (flags.carry)
    CC -> not <$> use (flags.carry)
    MI -> use (flags.negative)
    PL -> not <$> use (flags.negative)
    VS -> use (flags.overflow)
    VC -> not <$> use (flags.overflow)
    HI -> (&&) <$> use (flags.carry) <*> (not <$> use (flags.zero))
    LS -> (||) <$> (not <$> use (flags.carry)) <*> use (flags.zero)
    GE -> (==) <$> use (flags.negative) <*> use (flags.overflow)
    LT -> (/=) <$> use (flags.negative) <*> use (flags.overflow)
    GT ->
      (&&) <$> (not <$> use (flags.zero))
           <*> ((==) <$> use (flags.negative) <*> use (flags.overflow))
    LE ->
      (||) <$> (not <$> use (flags.zero))
           <*> ((/=) <$> use (flags.negative) <*> use (flags.overflow))
    AL -> return True
