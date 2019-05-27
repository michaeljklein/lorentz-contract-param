
module Gas.Cost.Typecheck where

import Prelude hiding (EQ, LT, GT)

import Gas.Type
import qualified Gas.Script as Script
import Michelson.Typed

type_ = allocCost . (+ 1)
cycle = stepCost 1

unit = free
bool = free

string = allocBytesCost
z      = allocBitsCost . fromIntegral . Script.significantBitCount

tez              = stepCost 1 <> allocCost 1
string_timestamp = stepCost 3 <> allocCost 3
key              = stepCost 3 <> allocCost 3
key_hash         = stepCost 1 <> allocCost 1
signature        = stepCost 1 <> allocCost 1

contract = stepCost 5

pair   = allocCost 2
union  = allocCost 1
lambda = allocCost 5 <> stepCost 3

some = allocCost 1
none = allocCost 0

contractExists = stepCost 15 <> allocCost 5
getScript      = stepCost 15 <> allocCost 5

list_element     = allocCost 2 <> stepCost 1
map_element size = fromIntegral (Script.log2 size) `scale` (allocCost 4 <> stepCost 2)
set_element size = fromIntegral (Script.log2 size) `scale` (allocCost 3 <> stepCost 2)

instr = \case
  Seq {} -> allocCost 8
  Nop {} -> allocCost 0
  Ext {} -> error "TODO: Find proper gas cost for Ext instr"
  Nested {} -> allocCost 0
  DROP {} -> allocCost 0
  DUP {} -> allocCost 1
  SWAP {} -> allocCost 0
  PUSH {} -> allocCost 0
  SOME {} -> allocCost 2
  NONE {} -> allocCost 3
  UNIT {} -> allocCost 1
  IF_NONE {} -> allocCost 0
  PAIR {} -> allocCost 2
  CAR {} -> allocCost 1
  CDR {} -> allocCost 1
  LEFT {} -> allocCost 3
  RIGHT {} -> allocCost 3
  IF_LEFT {} -> allocCost 2
  NIL {} -> allocCost 1
  CONS {} -> allocCost 1
  IF_CONS {} -> allocCost 2
  SIZE {} -> allocCost 1
  EMPTY_SET {} -> allocCost 1
  EMPTY_MAP {} -> allocCost 2
  MAP {} -> allocCost 5
  ITER {} -> allocCost 4
  MEM {} -> allocCost 1
  GET {} -> allocCost 1
  UPDATE {} -> allocCost 1
  IF {} -> allocCost 8
  LOOP {} -> allocCost 4
  LOOP_LEFT {} -> allocCost 5
  LAMBDA {} -> allocCost 2
  EXEC {} -> allocCost 1
  DIP {} -> allocCost 4
  FAILWITH {} -> allocCost 1
  CAST {} -> error "TODO: Find CAST instruction"
  RENAME {} -> error "TODO: Find RENAME instruction"
  PACK {} -> allocCost 2
  UNPACK {} -> allocCost 2
  CONCAT {} -> allocCost 1
  CONCAT' {} -> error "TODO: Find proper gas cost for RENAME instruction"
  SLICE {} -> allocCost 1
  ISNAT {} -> allocCost 1
  ADD {} -> allocCost 1
  SUB {} -> allocCost 1
  MUL {} -> allocCost 1
  EDIV {} -> allocCost 1
  ABS {} -> allocCost 1
  NEG {} -> allocCost 1
  LSL {} -> allocCost 1
  LSR {} -> allocCost 1
  OR {} -> allocCost 1
  AND {} -> allocCost 1
  XOR {} -> allocCost 1
  NOT {} -> allocCost 1
  COMPARE {} -> allocCost 1
  EQ {} -> allocCost 1
  NEQ {} -> allocCost 1
  LT {} -> allocCost 1
  GT {} -> allocCost 1
  LE {} -> allocCost 1
  GE {} -> allocCost 1
  INT {} -> allocCost 1
  SELF {} -> allocCost 2
  CONTRACT {} -> allocCost 2
  TRANSFER_TOKENS {} -> allocCost 1
  SET_DELEGATE {} -> allocCost 1
  CREATE_ACCOUNT {} -> allocCost 2
  CREATE_CONTRACT {} -> allocCost 8
  IMPLICIT_ACCOUNT {} -> allocCost 1
  NOW {} -> allocCost 1
  AMOUNT {} -> allocCost 1
  BALANCE {} -> allocCost 1
  CHECK_SIGNATURE {} -> allocCost 1
  SHA256 {} -> allocCost 1
  SHA512 {} -> allocCost 1
  BLAKE2B {} -> allocCost 1
  HASH_KEY {} -> allocCost 1
  STEPS_TO_QUOTA {} -> allocCost 1
  SOURCE {} -> allocCost 1
  SENDER {} -> allocCost 1
  ADDRESS {} -> allocCost 1
