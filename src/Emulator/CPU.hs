module Emulator.CPU where

import Emulator.Types

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
