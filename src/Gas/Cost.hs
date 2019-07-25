-- | Module with gas cost calculation helpers that are used
-- in the other modules. Ported from here:
-- https://github.com/BrianGuo/MichelsonExecutionEngine/blob/master/util/gas_costs.ml#L8
module Gas.Cost
  ( bytes
  , string
  , zint
  )where

import Gas.Type
import qualified Gas.Script as Script

bytes, string, zint :: Word64 -> Cost
bytes  = allocMBytesCost
string = allocBytesCost
zint   = allocBitsCost . Script.significantBitCount

