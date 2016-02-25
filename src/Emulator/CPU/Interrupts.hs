module Emulator.CPU.Interrupts where


-- Before calling an interrupt, the CPU must perform some housekeeping
-- on various things before handing control back over to the ROM.
-- NOTE: This function should be called with PC still being the OLD
-- address, otherwise we don't know where to return to when we're done
enterInterrupt :: (MonadState Registers m, MonadState Memory m) => m ()
-- TODO: Store PC+nn (where nn is the pipelining number, different for ARM and THUMB)
--       save flags
--       set ARM state flags
--       disable IRQs and FIQs
--       jump to correct interrupt vector
enterInterrupt = undefined
