module Emulator.CPU
  ( CPUMode(..)
  , Flags()
  , mkFlags
  , HasFlags(..)
  , HasNegative(..)
  , HasZero(..)
  , HasCarry(..)
  , HasOverflow(..)
  , HasIrqDisable(..)
  , HasFiqDisable(..)
  , HasThumbStateBit(..)
  -- , applyFlags
  -- , extractFlags
  , Registers()
  , mkRegisters
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
  , applyShiftType
  , registerLens
  , rn
  , shiftedRegisterLens
  , operand2Lens
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
import Data.Int
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
  { -- condition code flags
    _flagsNegative :: Bool -- ^ negative (bit 31)
  , _flagsZero :: Bool -- ^ zero (bit 30)
  , _flagsCarry :: Bool -- ^ carry (bit 29)
  , _flagsOverflow :: Bool -- ^ overflow (bit 28)
   -- bits 8:27 are reserved
   -- control bits
  , _flagsIrqDisable :: Bool -- ^ irqDisable (bit 7)
  , _flagsFiqDisable :: Bool -- ^ fiqDisable (bit 6)
  , _flagsThumbStateBit :: Bool } -- ^ thumbStateBit (bit 5)
   -- TODO: missing mode bits!
  deriving (Show, Read, Eq)

makeLensesWith defaultFieldRules ''Flags

class HasFlags a where
  flags :: Lens' a Flags

instance HasFlags Flags where
  flags = iso id id

instance Default Flags where
  def = Flags False False False False False False False

mkFlags :: Bool -- ^ negative
        -> Bool -- ^ zero
        -> Bool -- ^ carry
        -> Bool -- ^ overflow
        -> Bool -- ^ irqDisable
        -> Bool -- ^ fiqDisable
        -> Bool -- ^ thumbStateBit
        -> Flags
mkFlags = Flags

applyFlags :: Flags -> MWord -> MWord
applyFlags f w =
  w & bitAt 31 .~ (f ^. negative)
    & bitAt 30 .~ (f ^. zero)
    & bitAt 29 .~ (f ^. carry)
    & bitAt 28 .~ (f ^. overflow)
    & bitAt 7 .~ (f ^. irqDisable)
    & bitAt 6 .~ (f ^. fiqDisable)
    & bitAt 5 .~ (f ^. thumbStateBit)

extractFlags :: MWord -> Flags
extractFlags =
  Flags <$> tb 31
        <*> tb 30
        <*> tb 29
        <*> tb 28
        <*> tb 7
        <*> tb 6
        <*> tb 5
  where tb = flip testBit

-- | The 'Registers' data type keeps track of the ARM7 processor registers. Each register
--     is a 32-bit word-sized value, and some of the registers have specific purposes: 'r13'
--     is the stack pointer; 'r14' is the link register; and 'r15' is the program counter.
--     It also keeps track of the CPSR register (also known as the flags register), which
--     handles various different status flags which are modified by instructions and determine
--     which of the decoded instructions will actually be executed (the instruction conditions).
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
  , _registersR13 :: MWord -- Stack Pointer (SP)
  , _registersR14 :: MWord -- Link Register (LR)
  , _registersR15 :: MWord -- Program Counter (PC)
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

-- | Create a 'Registers' value. This is literally just the 'Registers' constructor, except
--     that's hidden. Typically, you'll want to create a 'Registers' value with 'def', though,
--     since 'mkRegisters' can be confusing with the number of different fields with the same
--     type.
mkRegisters :: MWord -- ^ 'r0' field
            -> MWord -- ^ 'r1' field
            -> MWord -- ^ 'r2' field
            -> MWord -- ^ 'r3' field
            -> MWord -- ^ 'r4' field
            -> MWord -- ^ 'r5' field
            -> MWord -- ^ 'r6' field
            -> MWord -- ^ 'r7' field
            -> MWord -- ^ 'r8' field
            -> MWord -- ^ 'r9' field
            -> MWord -- ^ 'r10' field
            -> MWord -- ^ 'r11' field
            -> MWord -- ^ 'r12' field
            -> MWord -- ^ 'r13' field (the stack pointer field)
            -> MWord -- ^ 'r14' field (the link field, used for returning from subroutines)
            -> MWord -- ^ 'r15' field (the program counter field)
            -> Flags -- ^ 'cpsr' field (the flags field)
            -> Registers
mkRegisters = Registers

-- | Shifted registers in instructions can be shifted by one of four methods.
data ShiftType = LogicalLeft -- ^ a logical left shift
               | LogicalRight -- ^ a logical right shift
               | ArithRight -- ^ an arithmetic right shift (the word is treated as a twos-complement integer)
               | RotateRight -- ^ a logical right rotate
  deriving (Show, Read, Eq, Enum, Bounded)

-- | Parses a 'ShiftType' from a given byte, and if the given byte is invalid simply
--     returns 'Nothing'.
shiftTypeFromByte :: Byte -> Maybe ShiftType
shiftTypeFromByte = fromByte

-- | Apply the given 'ShiftType' to the given machine word with the supplied magnitude.
applyShiftType :: ShiftType -- ^ shift type to use
               -> MWord -- ^ the machine word which should be shifted
               -> Int -- ^ how many bits the machine word should be shifted by
               -> MWord
applyShiftType st x s =
  case st of
    LogicalLeft -> x `shiftL` s
    LogicalRight -> x `shiftR` s
    ArithRight -> fromIntegral $ (fromIntegral x :: Int32) `shiftR` s
    RotateRight -> x `rotateR` s

-- | A 'Shifted' register is either shifted by a predefined byte value, or the value
--     in another register. It can also be shifted in a variety of ways.
data Shifted a = AmountShift Byte ShiftType a -- ^ shift the register by a fixed number of bytes according to the shift type
               | RegisterShift a ShiftType a -- ^ shift the register by the value of another specified register (according to the shift type)
  deriving (Show, Read, Eq, Functor)

-- | Given a 'RegisterName', which is simply a wrapped 'Int' (which should be 0-15),
--     'registerLens' returns the corresponding lens which can be used to access that
--     particular register.
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

-- | 'rn' is just a shorter name for 'registerLens' which is easier to use when writing
--     instruction handling code.
rn :: RegisterName -> Lens' Registers MWord
rn = registerLens

shiftedRegisterLens :: Shifted RegisterName -> Getter Registers MWord
shiftedRegisterLens (AmountShift byte shiftType regName) =
  registerLens regName.(to (\x -> applyShiftType shiftType x (fromIntegral byte)))
shiftedRegisterLens (RegisterShift shiftReg shiftType regName) =
  to $ \r -> do
    let val = r ^. registerLens regName
    let offset = fromIntegral $ r ^. registerLens shiftReg :: Byte
    applyShiftType shiftType val (fromIntegral offset)

operand2Lens :: Either (Shifted RegisterName) (Rotated Byte) -> Getter Registers MWord
operand2Lens (Left r) = shiftedRegisterLens r
operand2Lens (Right (Rotated x b)) = to $ const $ fromIntegral b `rotateL` (x * 2)

fromByte :: forall a. (Enum a, Bounded a) => Byte -> Maybe a
fromByte b =
  if fromIntegral b > maxByte then Nothing else Just $ toEnum $ fromIntegral b
    where
      maxByte = fromEnum (maxBound :: a)

-- | Conditions which determine whether each instruction should be executed.
--     Each condition checks the current CPSR state and decides whether the
--     corresponding instruction should run.
data Condition = EQ -- ^ Equal
               | NE -- ^ Not equal
               | CS -- ^ Carry set
               | CC -- ^ Carry clear
               | MI -- ^ Negative
               | PL -- ^ Positive
               | VS -- ^ Overflow
               | VC -- ^ No overflow
               | HI -- ^ Unsigned higher
               | LS -- ^ Unsigned lower or same
               | GE -- ^ Signed greater than or equal
               | LT -- ^ Less than
               | GT -- ^ Greater than
               | LE -- ^ Less than or equal
               | AL -- ^ Always (unconditional)
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Parses a 'Condition' from a given byte, and if the given byte is invalid simply
--     returns 'Nothing'.
conditionFromByte :: Byte -> Maybe Condition
conditionFromByte = fromByte

-- | 'Opcode' represents the various different opcodes that are supported by the ARM
--     data processing instruction.
data Opcode = AND -- ^ Rd := Op1 AND Op2
            | EOR -- ^ Rd := Op1 EOR Op2
            | SUB -- ^ Rd := Op1 - Op2
            | RSB -- ^ Rd := Op2 - Op1
            | ADD -- ^ Rd := Op1 + Op2
            | ADC -- ^ Rd := Op1 + Op2 + C
            | SBC -- ^ Rd := Op1 - Op2 + C - 1
            | RSC -- ^ Rd := Op2 - Op1 + C - 1
            | TST -- ^ set condition codes on Op1 AND Op2
            | TEQ -- ^ set condition codes on Op1 EOR Op2
            | CMP -- ^ set condition codes on Op1 - Op2
            | CMN -- ^ set condition codes on Op1 + Op2
            | ORR -- ^ Rd := Op1 OR Op2
            | MOV -- ^ Rd := Op2
            | BIC -- ^ Rd := Op1 AND NOT Op2
            | MVN -- ^ Rd := NOT Op2
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Parses an 'Opcode' from a given byte, and if the given byte is invalid simply
--     returns 'Nothing'.
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
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Parses a 'ThumbOpcode' from a given byte, and if the given byte is invalid simply
--     returns 'Nothing'.
thumbOpcodeFromByte :: Byte -> Maybe ThumbOpcode
thumbOpcodeFromByte = fromByte

data Interrupt = Reset                -- Probably won't be used
               | UndefinedInstruction -- Used if a bad instruction is found
               | SoftwareInterrupt    -- Interrupts that can be generated by the programmer
               | PrefetchAbort        -- The *Abort interrupts probably won't be used as they are
               | DataAbort            --   for virtual memory systems, which the GBA doesn't use.
               | NormalInterrupt      -- Standard interrupts
               | FastInterrupt        -- Fast (standard) interrupts
   deriving (Show, Read, Eq, Ord)
