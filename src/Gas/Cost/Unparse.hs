-- | Module which contains costs for instructions and values unparsing
-- (translating from typed representation to untyped) ported from OCaml
-- implementation located here:
-- https://github.com/BrianGuo/MichelsonExecutionEngine/blob/master/util/gas_costs.ml#L25
module Gas.Cost.Unparse
  ( cycle
  , primCost
  , stringCost
  , seqCost

  , unit, int
  , string, bytes
  , bool, timestamp
  , contract, signature
  , tez, key, keyHash
  , operation, pair, union
  , some, none

  , listElement, setElement, mapElement
  )where

import Prelude hiding (bool, cycle, some)

import Gas.Type
import qualified Gas.Script as Script
import Michelson.Untyped (InternalByteString)
import Tezos.Core (Timestamp)

cycle :: Cost
cycle      = stepCost 1

primCost :: Int -> [Text] -> Cost
primCost = Script.primNodeCostNonrecOfLength

stringCost :: Int -> Cost
stringCost = Script.stringNodeCostOfLength

seqCost :: Int -> Cost
seqCost = Script.seqNodeCostNonrecOfLength

unit :: Cost
unit = primCost 0 []

int :: Int -> Cost
int = Script.intNodeCost

string :: Text -> Cost
string = Script.stringNodeCost

bytes :: InternalByteString -> Cost
bytes = Script.bytesNodeCost

bool :: Cost
bool = primCost 0 []

timestamp :: Timestamp -> Cost
timestamp  = error "convert ts -> int properly"

contract, signature, tez, key, keyHash :: Cost
contract = stringCost 36
signature = stringCost 128
tez = Script.intNodeCostOfNumbits 60
key        = stringCost 54
keyHash    = stringCost 36

operation :: InternalByteString -> Cost
operation  = Script.bytesNodeCost

pair, union, some, none :: Cost
pair       = primCost 2 []
union      = primCost 1 []
some       = primCost 1 []
none       = primCost 0 []

listElement, setElement, mapElement :: Cost
listElement = allocCost 2
setElement  = allocCost 2
mapElement  = allocCost 2
