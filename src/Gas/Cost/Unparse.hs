
module Gas.Cost.Unparse where

import Gas.Type
import qualified Gas.Script as Script

cycle      = stepCost 1
primCost   = Script.primNodeCostNonrecOfLength
stringCost = Script.stringNodeCostOfLength
seqCost    = Script.seqNodeCostNonrecOfLength

unit       = primCost 0 []
int        = Script.intNodeCost
string     = Script.stringNodeCost
bytes      = Script.bytesNodeCost
bool       = primCost 0 []
timestamp  = error "convert ts -> int properly"
contract   = stringCost 36
signature  = stringCost 128
tez        = Script.intNodeCostOfNumbits 60
key        = stringCost 54
keyHash    = stringCost 36
operation  = Script.bytesNodeCost
pair       = primCost 2 []
union      = primCost 1 []
some       = primCost 1 []
none       = primCost 0 []

listElement = allocCost 2
setElement  = allocCost 2
mapElement  = allocCost 2
