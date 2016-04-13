module Emulator.CPU.Instructions.ARM.Opcodes
  ( functionFromOpcode
  , and
  , eor
  , sub
  , rsb
  , add
  , adc
  , sbc
  , rsc
  , tst
  , teq
  , cmp
  , cmn
  , orr
  , mov
  , bic
  , mvn
  , checkCarry
  , isNegative )
  where

import Emulator.Types
import Emulator.CPU

import Control.Lens
import Control.Monad.State.Class
import Control.Monad
import Data.Bits
import Data.Int (Int32)
import Prelude hiding (Ordering(..), and)

type SrcRegister = Getting MWord Registers MWord
type DestRegister = ASetter' Registers MWord
type ConditionCode = Bool

functionFromOpcode :: (HasFlags s, HasRegisters s, MonadState s m)
                   => Opcode -> DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
functionFromOpcode opcode =
  case opcode of
    AND -> and
    EOR -> eor
    SUB -> sub
    RSB -> rsb
    ADD -> add
    ADC -> adc
    SBC -> sbc
    RSC -> rsc
    TST -> tst
    TEQ -> teq
    CMP -> cmp
    CMN -> cmn
    ORR -> orr
    MOV -> mov
    BIC -> bic
    MVN -> mvn

---------------------
-- Data processing instructions
-- TODO: almost all flags need checking/fixing
---------------------

-- Standard arithmetic add
add :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
add dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 + res2
  registers.dest .= val
  -- Update flags if the condition is true
  when cCode $ do
    flags.negative .= isNegative val
    flags.zero .= (val == 0)
    flags.carry .= isUnsignedOverflow (+) [res1, res2] val
    flags.overflow .= isSignedOverflow (+) [res1, res2] val

-- Arithmetic add with carry
adc :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
adc dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  isCy <- use $ flags.carry
  let cy = if isCy then 1 else 0
  let val = res1 + res2 + cy
  registers.dest .= val
  when cCode $ do
    flags.negative .= isNegative val
    flags.zero .= (val == 0)
    flags.carry .= isUnsignedOverflow (+) [res1, res2, cy] val
    flags.overflow .= isSignedOverflow (+) [res1, res2, cy] val

-- Arithmetic subtract
sub :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
sub dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 - res2
  registers.dest .= val
  when cCode $ do
    flags.negative .= isNegative val
    flags.zero .= (val == 0)
    flags.carry .= isUnsignedOverflow (-) [res1, res2] val
    flags.overflow .= isSignedOverflow (-) [res1, res2] val

-- Subtract with carry
sbc :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
sbc dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  isCy <- use $ flags.carry
  -- SBC uses NOT(Carry)
  let cy = if isCy then 0 else 1
  let val = res1 + res2 + cy
  registers.dest .= val
  when cCode $ do
    flags.negative .= isNegative val
    flags.zero .= (val == 0)
    flags.carry .= isUnsignedOverflow (-) [res1, res2, cy] val
    flags.overflow .= isSignedOverflow (-) [res1, res2, cy] val

-- Subtract with carry reversed
rsc :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
rsc dest src1 src2 cCode = sbc dest src2 src1 cCode

-- Arithmetic subtract reversed
rsb :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
rsb dest src1 src2 cCode = sub dest src2 src1 cCode

-- Logical/bitwise AND
and :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
and dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 .&. res2
  registers.dest .= val
  when cCode $ do
    flags.negative .= isNegative val
    flags.zero .= (val == 0)
    -- TODO: carry from barrel shifter
    flags.carry .= False

-- Logical/bitwise OR
orr :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
orr dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 .|. res2
  registers.dest .= val
  when cCode $ do
    flags.negative .= isNegative val
    flags.zero .= (val == 0)
    -- TODO: carry from barrel shifter
    flags.carry .= False

-- Move Negative (aka Move NOT)
mvn :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
mvn dest _ src2 cCode = do
  res2 <- use $ registers.src2
  let val = complement res2
  registers.dest .= val
  when cCode $ do
    flags.negative .= isNegative val
    flags.zero .= (val == 0)
    -- TODO: carry from barrel shifter
    flags.carry .= False

-- Bit clear
bic :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
bic dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 .&. (complement res2)
  registers.dest .= val
  when cCode $ do
    flags.negative .= isNegative val
    flags.zero .= (val == 0)
    -- TODO: carry from barrel shifter
    flags.carry .= False

-- Test instruction
tst :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
tst _ src1 src2 _ = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 .&. res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  flags.negative .= isNegative val
  flags.zero .= (val == 0)
  -- TODO: carry from barrel shifter
  flags.carry .= False

-- Test exclusive (XOR)
teq :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
teq _ src1 src2 _ = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 `xor` res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  flags.negative .= isNegative val
  flags.zero .= (val == 0)
  -- TODO: carry from barrel shifter
  flags.carry .= False

-- Compare
cmp :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
cmp _ src1 src2 _ = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 - res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  flags.zero .= (val == 0)
  flags.negative .= isNegative val
  flags.carry .= not (isUnsignedOverflow (-) [res1, res2] val)
  flags.overflow .= isSignedOverflow (-) [res1, res2] val

-- Compare negative
cmn :: (HasFlags s, HasRegisters s, MonadState s m)
    => a -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
cmn _ src1 src2 _ = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 + res2
  -- Always update flags (for TST, TEQ, CMP, CMN)
  flags.zero .= (val == 0)
  flags.negative .= isNegative val
  flags.carry .= not (isUnsignedOverflow (-) [res1, res2] val)
  flags.overflow .= isSignedOverflow (-) [res1, res2] val

-- Logical Exclusive Or
eor :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
eor dest src1 src2 cCode = do
  res1 <- use $ registers.src1
  res2 <- use $ registers.src2
  let val = res1 `xor` res2
  registers.dest .= val
  when cCode $ do
    flags.zero .= (val == 0)
    flags.negative .= isNegative val
    -- TODO: carry from barrel shifter
    flags.carry .= False

-- Move instruction
mov :: (HasFlags s, HasRegisters s, MonadState s m)
    => DestRegister -> SrcRegister -> SrcRegister -> ConditionCode -> m ()
mov dest _ src2 cCode = do
  val <- use $ registers.src2
  registers.dest .= val
  when cCode $ do
    -- val is expected to be post-shift for the flags!
    flags.zero .= (val == 0)
    flags.negative .= isNegative val
    -- TODO: carry from barrel shifter
    flags.carry .= False

checkCarry :: MWord -> MWord -> Bool
checkCarry a b = ((c .&. 0x00000000FFFFFFFF) `xor` c) /= 0
  where
    c :: DWord
    c = fromIntegral a + fromIntegral b

isNegative :: MWord -> Bool
isNegative a = testBitDefault a 31

-- Following functions take lists for cases Rm + Rn + Carry
isUnsignedOverflow :: (a ~ Integer) => (a -> a -> a) -> [MWord] -> MWord -> Bool
isUnsignedOverflow f args result = (fromIntegral result) /= preciseResult
  where
    preciseResult = foldl1 f $ map fromIntegral args :: Integer

isSignedOverflow :: (a ~ Int32) => (a -> a -> a) -> [MWord] -> MWord -> Bool
isSignedOverflow f args result = (fromIntegral signedResult :: Integer) /= (fromIntegral result :: Integer)
  where
    signedResult = foldl1 f $ map fromIntegral args :: Int32
