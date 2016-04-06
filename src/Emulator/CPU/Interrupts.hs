module Emulator.CPU.Interrupts where

import Emulator.Memory

import Data.Bits

data Interrupt = LCDVBlank
               | LCDHBlank -- The current design plan will not allow this, but keeping it in for consistency
                           -- and so we can use fromEnum when indexing the IO reg
               | LCDVCountMatch
               | Timer0Overflow
               | Timer1Overflow
               | Timer2Overflow
               | Timer3Overflow
               | SerialCommunication -- Definitely won't be used!
               | DMA0
               | DMA1
               | DMA2
               | DMA3
               | Keypad -- Documentation describes this as too slow for games, so also probably won't be used
               | GamePakSrc
      deriving (Show, Eq, Enum)

-- Before calling an interrupt, the CPU must perform some housekeeping
-- on various things before handing control back over to the ROM.
-- NOTE: This function should be called with PC still being the OLD
-- address, otherwise we don't know where to return to when we're done
enterInterrupt :: AddressSpace m => m ()
-- TODO: Store PC+nn (where nn is the pipelining number, different for ARM and THUMB)
--       save flags
--       set ARM state flags
--       disable IRQs and FIQs
--       jump to correct interrupt vector
enterInterrupt = undefined

interruptsEnabled :: AddressSpace m => m Bool
interruptsEnabled = do
  val <- readAddressHalfWord 0x4000208
  return $ testBit val 0

interruptToBitIndex :: Interrupt -> Int
interruptToBitIndex = fromEnum

interruptEnabled :: AddressSpace m => Interrupt -> m Bool
interruptEnabled i = do
  val <- readAddressHalfWord 0x4000200
  return $ testBit val $ interruptToBitIndex i
