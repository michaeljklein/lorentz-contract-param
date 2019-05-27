
module Gas.Cost where

import Gas.Type
import qualified Gas.Script as Script

bytes  = allocMBytesCost
string = allocBytesCost
zint   = allocBitsCost . Script.significantBitCount

