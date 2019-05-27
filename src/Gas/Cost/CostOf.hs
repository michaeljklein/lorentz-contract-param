
module Gas.Cost.CostOf where

import qualified Data.ByteString as BS

import Gas.Cost
import Gas.Type
import qualified Gas.Script as Script

zToInt64 = stepCost 2 <> allocCost 1
set_access _key = error "TODO: find set of the Box"
set_update key _presence set = set_access key set <> allocCost 3
hash data_ len = 10 `scale` stepCost (fromIntegral $ BS.length data_) <> bytes len
