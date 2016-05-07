module Emulator.Interpreter
  ( interpretLoop ) where

import Emulator.CPU
import Emulator.CPU.Instructions
import Emulator.Debug
import Emulator.Interpreter.ARM
import Emulator.Interpreter.Monad
import Emulator.Interpreter.THUMB
import Emulator.Memory
import Emulator.Types
import Utilities.Show

import Control.Concurrent.STM
import Control.Concurrent.STM.TXChan
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Text.Format hiding (print)

interpretLoop :: TXChan SystemState -> MonadIO m => SystemT m ()
interpretLoop chan = do
  _debugger (interpretLoop chan)
  get >>= liftIO . atomically . writeTXChan chan
  isTHUMB <- use (registers.flags.thumbStateBit)
  if isTHUMB
    then do
      sysRegisters.r15 += 2
      pc <- prefetchedTHUMB <$> use (sysRegisters.r15) -- adjusted for prefetch
      newInstr <- readAddressHalfWord pc
      --when (newInstr == 0x0000) $ error "probably broken instruction"
      case parseTHUMB newInstr of
        Left err -> do
          debug Error $ format "{} {} {}" (showHex pc, showHex newInstr, show err)
          error $ "interpretLoop: instruction parse failed (" ++ err ++ ")"
        Right instr -> do
          debug Info $ format "{THUMB}: pc: {}, instr: {}\n        {}"
            (showHex pc, showHex newInstr, show instr)
          interpretThumb instr
          interpretLoop chan
    else do
      sysRegisters.r15 += 4
      pc <- prefetchedARM <$> use (sysRegisters.r15) -- adjusted for prefetch
      newInstr <- readAddressWord pc
      --when (newInstr == 0x00000000) $ error "probably broken instruction"
      case parseARM newInstr of
        Left err -> do
          debug Error $ format "{} {} {}" (showHex pc, showHex newInstr, show err)
          error $ "interpretLoop: instruction parse failed (" ++ err ++ ")"
        Right (cond, instr) -> do
          shouldRun <- runCondition cond
          debug Info $ format "{ARM}: addr: {}, instr: {}, condition: {} ({})\n        {}"
            (showHex pc, showHex newInstr, show cond, yesNo shouldRun, show instr)
          conditionally cond $ interpretARM instr
          interpretLoop chan

prefetchedARM :: Address -> Address
prefetchedARM addr = addr - 8

prefetchedTHUMB :: Address -> Address
prefetchedTHUMB addr = addr - 4

yesNo :: Bool -> String
yesNo True = "yes"
yesNo False = "no"

_debugRegisters :: MonadIO m => SystemT m ()
_debugRegisters = do
  rs <- mapM use $ map (registers.) [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15]
  debug Warning $ format "{} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}" (map (showHexPadded 8) rs)

_debugger :: MonadIO m => SystemT m () -> SystemT m ()
_debugger act = do
  line <- liftIO getLine
  case line of
    "" -> return ()
    "r" -> do
      _debugRegisters
      act
    "s" -> do
      f <- use flags
      liftIO $ print f
      act
    _ -> return ()
