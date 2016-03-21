module Emulator.CPU
  ( CPUMode(..)
  , Flags(Flags)
  , HasFlags(..)
  , HasSign(..)
  , HasZero(..)
  , HasCarry(..)
  , HasOverflow(..)
  , HasStickyOverflow(..)
  , HasIrqDisable(..)
  , HasFiqDisable(..)
  , HasStateBit(..)
  , applyFlags
  , extractFlags
  , Registers(Registers)
  , HasRegisters(..)
  , HasR0(..)
  , HasR1(..)
  , HasR2(..)
  , HasR3(..)
  , HasR4(..)
  , HasR5(..)
  , HasR6(..)
  , HasR7(..)
  , HasR8(..)
  , HasR9(..)
  , HasR10(..)
  , HasR11(..)
  , HasR12(..)
  , HasR13(..)
  , HasR14(..)
  , HasR15(..)
  , HasCpsr(..)
  , ShiftType(..)
  , shiftTypeFromByte
  , Shifted(..)
  , Condition(..)
  , conditionFromByte
  , Opcode(..)
  , opcodeFromByte
  , Interrupt(..)
  , ThumbOpcode(..)
  , thumbOpcodeFromByte ) where

import Emulator.Types

import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Default.Class

data CPUMode = User
             | FIQ
             | IRQ
             | Supervisor
             | Abort
             | Undefined
             | System
  deriving (Show, Read, Eq)

data Flags = Flags -- Status Register
  { _flagsSign :: Bool
  , _flagsZero :: Bool
  , _flagsCarry :: Bool
  , _flagsOverflow :: Bool
  , _flagsStickyOverflow :: Bool
  , _flagsIrqDisable :: Bool
  , _flagsFiqDisable :: Bool
  , _flagsStateBit :: Bool }
  deriving (Show, Read, Eq)

makeLensesWith defaultFieldRules ''Flags

class HasFlags a where
  flags :: Lens' a Flags

instance HasFlags Flags where flags = iso id id

instance Default Flags where
  def = Flags False False False False False False False False

applyFlags :: Flags -> MWord -> MWord
applyFlags f w =
  w & bitAt 31 .~ (f ^. sign)
    & bitAt 30 .~ (f ^. zero)
    & bitAt 29 .~ (f ^. carry)
    & bitAt 28 .~ (f ^. overflow)
    & bitAt 27 .~ (f ^. stickyOverflow)
    & bitAt 7 .~ (f ^. irqDisable)
    & bitAt 6 .~ (f ^. fiqDisable)
    & bitAt 5 .~ (f ^. stateBit)

extractFlags :: MWord -> Flags
extractFlags =
  Flags <$> tb 31
        <*> tb 30
        <*> tb 29
        <*> tb 28
        <*> tb 27
        <*> tb 7
        <*> tb 6
        <*> tb 5
  where tb = flip testBit

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

class HasRegisters a where
  registers :: Lens' a Registers

instance HasRegisters Registers where
  registers = iso id id

instance Default Registers where
  def = Registers 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 def

instance HasFlags Registers where
  flags = cpsr

data ShiftType = LogicalLeft
               | LogicalRight
               | ArithRight
               | RotateRight
  deriving (Show, Read, Eq, Enum, Bounded)

shiftTypeFromByte :: Byte -> Maybe ShiftType
shiftTypeFromByte = fromByte

applyShiftType :: Bits a => ShiftType -> a -> Int -> a
applyShiftType st x s =
  case st of
    LogicalLeft -> x `shiftL` s
    LogicalRight -> x `shiftR` s
    ArithRight -> undefined
    RotateRight -> x `rotateR` s

data Shifted a = AmountShift Byte ShiftType a
               | RegisterShift a ShiftType a
  deriving (Show, Read, Eq)

registerLens :: RegisterName -> Lens' Registers MWord
registerLens (RegisterName n) =
  case n of
    0 -> r0
    1 -> r1
    2 -> r2
    3 -> r3
    4 -> r4
    5 -> r5
    6 -> r6
    7 -> r7
    8 -> r8
    9 -> r9
    10 -> r10
    11 -> r11
    12 -> r12
    13 -> r13
    14 -> r14
    15 -> r15
    _ -> error $ "registerLens: undefined register label: #" ++ show n

shiftedRegisterLens :: Shifted RegisterName -> Getter Registers ()
shiftedRegisterLens (AmountShift byte shiftType regName) = undefined
shiftedRegisterLens (RegisterShift shiftReg shiftType regName) = undefined

fromByte :: forall a. (Enum a, Bounded a) => Byte -> Maybe a
fromByte b =
  if fromIntegral b > maxByte then Nothing else Just $ toEnum $ fromIntegral b
    where
      maxByte = fromEnum (maxBound :: a)

data Condition = EQ -- Equal
               | NE -- Not equal
               | CS -- Carry set
               | CC -- Carry clear
               | MI -- Negative
               | PL -- Positive
               | VS -- Overflow
               | VC -- No overflow
               | HI -- Unsigned higher
               | LS -- Unsigned lower or same
               | GE -- Signed greater than or equal
               | LT -- Less than
               | GT -- Greater than
               | LE -- Less than or equal
               | AL -- Always (unconditional)
  deriving (Show, Read, Eq, Enum, Bounded)

conditionFromByte :: Byte -> Maybe Condition
conditionFromByte = fromByte

data Opcode = AND -- Rd := Op1 AND Op2
            | EOR -- Rd := Op1 EOR Op2
            | SUB -- Rd := Op1 - Op2
            | RSB -- Rd := Op2 - Op1
            | ADD -- Rd := Op1 + Op2
            | ADC -- Rd := Op1 + Op2 + C
            | SBC -- Rd := Op1 - Op2 + C - 1
            | RSC -- Rd := Op2 - Op1 + C - 1
            | TST -- set condition codes on Op1 AND Op2
            | TEQ -- set condition codes on Op1 EOR Op2
            | CMP -- set condition codes on Op1 - Op2
            | CMN -- set condition codes on Op1 + Op2
            | ORR -- Rd := Op1 OR Op2
            | MOV -- Rd := Op2
            | BIC -- Rd := Op1 AND NOT Op2
            | MVN -- Rd := NOT Op2
  deriving (Show, Read, Eq, Enum, Bounded)

opcodeFromByte :: Byte -> Maybe Opcode
opcodeFromByte = fromByte

data ThumbOpcode = T_AND
                 | T_EOR
                 | T_LSL
                 | T_LSR
                 | T_ASR
                 | T_ADC
                 | T_SBC
                 | T_ROR
                 | T_TST
                 | T_NEG
                 | T_CMP
                 | T_CMN
                 | T_ORR
                 | T_MUL
                 | T_BIC
                 | T_MVN
                 | T_MOV -- Not an ALU opcode, but THUMB ones
                 | T_ADD
  deriving (Show, Read, Eq, Enum, Bounded)

thumbOpcodeFromByte :: Byte -> Maybe ThumbOpcode
thumbOpcodeFromByte = fromByte

data Interrupt = Reset                -- Probably won't be used
               | UndefinedInstruction -- Used if a bad instruction is found
               | SoftwareInterrupt    -- Interrupts that can be generated by the programmer
               | PrefetchAbort        -- The *Abort interrupts probably won't be used as they are
               | DataAbort            --   for virtual memory systems, which the GBA doesn't use.
               | NormalInterrupt      -- Standard interrupts
               | FastInterrupt        -- Fast (standard) interrupts
   deriving (Show, Read, Eq)
