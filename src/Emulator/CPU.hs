module Emulator.CPU where

import Emulator.Types

import Control.Lens

data RegisterX =
  -- Registers R0 - R12 are all general purpose registers
  -- In non-thumb mode all registers are available for any purpose
  -- In thumb mode only registers R0 - R7 are available, whereas
  --  R8 - R12 are only accessible from certain instructions
  R0 RegisterVal
  | R1 RegisterVal
  | R2 RegisterVal
  | R3 RegisterVal
  | R4 RegisterVal
  | R5 RegisterVal
  | R6 RegisterVal
  | R7 RegisterVal
  | R8 RegisterVal
  | R9 RegisterVal
  | R10 RegisterVal
  | R11 RegisterVal
  | R12 RegisterVal
  | R13 RegisterVal  -- SP Register
                     -- Stack pointer in thumb mode, SP or GP in ARM mode
  | R14 RegisterVal  -- LR Register
                     -- Return address after branch with
                     -- link (LB instruction), or GP in ARM
  | R15 RegisterVal  -- PC Register
                     -- Program counter, contains address to currently executing
                     -- statement.
  | CPSR RegisterVal -- Flags and CPU state register

data Flags = Flags
  { _flagsSign :: Bool
  , _flagsZero :: Bool
  , _flagsCarry :: Bool
  , _flagsOverflow :: Bool
  , _flagsStickyOverflow :: Bool
  , _flagsIrqDisable :: Bool
  , _flagsFiqDisable :: Bool
  , _flagsStateBit :: Bool }
  deriving (Show, Read, Eq)

makeFields ''Flags

data Registers = Registers
  { _registersR0 :: MWord
  , _registersR1 :: MWord
  , _registersR2 :: MWord
  , _registersR3 :: MWord
  , _registersR4 :: MWord
  , _registersR5 :: MWord
  , _registersR6 :: MWord
  , _registersR7 :: MWord
  , _registersR8 :: MWord
  , _registersR9 :: MWord
  , _registersR10 :: MWord
  , _registersR11 :: MWord
  , _registersR12 :: MWord
  , _registersR13 :: MWord
  , _registersR14 :: MWord
  , _registersR15 :: MWord
  , _registersCpsr :: Flags }
  deriving (Show, Read, Eq)

makeFields ''Registers
