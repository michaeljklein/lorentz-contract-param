module Gas.Cost.Typecheck
  ( eqTypeCost
  , eqStackCost
  , cycle
  , instrCost
  , seqCost
  , unit, bool

  , parseTypeCost, parseCTypeCost

  , string, z

  , tez, stringTimestamp, key
  , keyHash, signature

  , contract

  , pair, union, lambda

  , some, none

  , contractExists, getScript

  , listElement, mapElement, setElement
  ) where

import Prelude hiding (EQ, LT, GT, bool, cycle, some)

import Data.Singletons (Sing(..))

import Gas.Type
import qualified Gas.Script as Script
import Michelson.Typed (fromSingT, T(..))
import Michelson.Untyped (ExpandedInstr, InstrAbstract (..))

type_ :: Word64 -> Cost
type_ = allocCost . (+ 1)

cycle :: Cost
cycle = stepCost 1

unit, bool :: Cost
unit = free
bool = free

string, z :: Word64 -> Cost
string = allocBytesCost
z      = allocBitsCost . Script.significantBitCount

tez, stringTimestamp, key, keyHash, signature :: Cost
tez              = stepCost 1 <> allocCost 1
stringTimestamp = stepCost 3 <> allocCost 3
key              = stepCost 3 <> allocCost 3
keyHash         = stepCost 1 <> allocCost 1
signature        = stepCost 1 <> allocCost 1

contract :: Cost
contract = stepCost 5

pair, union, lambda :: Cost
pair   = allocCost 2
union  = allocCost 1
lambda = allocCost 5 <> stepCost 3

some, none :: Cost
some = allocCost 1
none = allocCost 0

contractExists, getScript :: Cost
contractExists = stepCost 15 <> allocCost 5
getScript      = stepCost 15 <> allocCost 5

listElement :: Cost
listElement = allocCost 2 <> stepCost 1

mapElement, setElement :: Word64 -> Cost
mapElement size = Script.log2 size `scale` (allocCost 4 <> stepCost 2)
setElement size = Script.log2 size `scale` (allocCost 3 <> stepCost 2)

eqTypeCost :: Sing (t :: T) -> Cost
eqTypeCost s = typeEqCostImpl (fromSingT s)

typeEqCostImpl :: T -> Cost
typeEqCostImpl = \case
  TOption t -> type_ 2 <> typeEqCostImpl t
  TList t -> type_ 2 <> typeEqCostImpl t
  TSet _ -> type_ 2
  TContract t -> type_ 2 <> typeEqCostImpl t
  TPair t1 t2 -> type_ 4 <> typeEqCostImpl t1 <> typeEqCostImpl t2
  TOr t1 t2 -> type_ 4 <> typeEqCostImpl t1 <> typeEqCostImpl t2
  TLambda t1 t2 -> type_ 4 <> typeEqCostImpl t1 <> typeEqCostImpl t2
  TMap _ t -> type_ 4 <> typeEqCostImpl t
  TBigMap _ t -> type_ 4 <> typeEqCostImpl t
  Tc _ -> type_ 1
  _ -> type_ 1

eqStackCost :: [T] -> Cost
eqStackCost s = mconcat $ map typeEqCostImpl s

parseTypeCost :: Sing (t :: T) -> Cost
parseTypeCost s = parseTypeCostImpl (fromSingT s)
  where
    parseTypeCostImpl :: T -> Cost
    parseTypeCostImpl t = cycle <> case t of
      Tc _ -> type_ 0
      TOption t -> parseTypeCostImpl t <> type_ 2
      TList t -> parseTypeCostImpl t <> type_ 1
      TSet _ -> parseCTypeCost <> type_ 1
      TContract t -> parseTypeCostImpl t <> type_ 1
      TPair t1 t2 -> parseTypeCostImpl t1 <> parseTypeCostImpl t2 <> type_ 2
      TOr t1 t2 -> parseTypeCostImpl t1 <> parseTypeCostImpl t2 <> type_ 2
      TLambda t1 t2 -> parseTypeCostImpl t1 <> parseTypeCostImpl t2 <> type_ 2
      TMap _ t -> parseCTypeCost <> parseTypeCostImpl t <> type_ 2
      TBigMap _ _ -> type_ 2
      _ -> type_ 0

parseCTypeCost :: Cost
parseCTypeCost = cycle <> type_ 0

seqCost :: Cost
seqCost = allocCost 8

instrCost :: ExpandedInstr -> Cost
instrCost i = allocCost 1 <> case i of
  EXT {} -> allocCost 0
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
