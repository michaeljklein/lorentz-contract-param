-- | Module which contains costs for different instructions interpretation.
-- Basically it is port from Michelson gas costs OCaml implementation which is
-- located here: https://github.com/BrianGuo/MichelsonExecutionEngine/blob/master/util/gas_costs.ml#L208
module Gas.Cost.CostOf
  ( setUpdate
  , mapUpdate
  , bigMapUpdate
  , hash
  , mapToList
  , setToList
  , emptySet
  , emptyMap
  , setMem
  , mapMem
  , bigMapMem
  , mapGet
  , bigMapGet
  , setSize
  , mapSize
  , listSize

  , stackOp
  , push
  , wrap
  , variantNoData
  , pair
  , pairAccess
  , cons

  , cycle
  , branch
  , loopCycle

  , addTimestamp
  , subTimestamp
  , diffTimestamps
  , boolBinOp
  , boolUnOp
  , abs, int, neg
  , add, sub, mul, div
  , zToInt64, int64ToZ, int64Op
  , shiftLeft, shiftRight
  , logOr, logAnd, logXor, logNot
  , compareBool, compareString
  , compareBytes, compareTez
  , compareInt, compareNat
  , compareKeyHash , compareTimestamp
  , compareAddress, compareRes

  , concatString
  , sliceString
  , concatBytes

  , exec
  , unpackFailed
  , address, contract, transfer
  , createAccount, createContract
  , implicitAccount
  , setDelegate, balance, now
  , checkSignature
  , source, sender, self, amount, hashKey
  , stepsToQuota
  ) where

import Prelude hiding (abs, div, cycle)
import qualified Prelude (div)

import Data.Map.Strict as Map (Map, size)
import Data.Set as Set (Set, size)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Foldable as Foldable

import Gas.Cost
import Gas.Type
import qualified Gas.Script as Script
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash)

class HasSize b where
  sizeOf :: b -> Word64

instance HasSize (Map k v) where
  sizeOf = fromIntegral . Map.size

instance HasSize (Set k) where
  sizeOf = fromIntegral . Set.size

zToInt64 :: Cost
zToInt64 = stepCost 2 <> allocCost 1

treeAccess :: HasSize a => a -> Word64
treeAccess = Script.log2 . sizeOf

setUpdate :: Set k -> Cost
setUpdate set' = treeAccess set' `scale` allocCost 3

mapUpdate :: Map k v -> Cost
mapUpdate map' = treeAccess map' `scale` allocCost 3

bigMapUpdate :: Map k v -> Cost
bigMapUpdate _ = stepCost 10

hash :: ByteString -> Word64 -> Cost
hash data_ len = 10 `scale`
  stepCost (fromIntegral $ BS.length data_)
  <> bytes len

mapToList :: Map k v -> Cost
mapToList = scale 3 . allocCost . sizeOf

setToList :: Set k -> Cost
setToList = scale 3 . allocCost . sizeOf


module_, cycle, stackOp, push, wrap, variantNoData, branch :: Cost
module_ = allocCost 10
cycle = stepCost 1
stackOp = stepCost 1
push = stepCost 1
wrap = allocCost 1
variantNoData = allocCost 1
branch = stepCost 2

pair, pairAccess, cons, loopCycle, listSize :: Cost
pair = allocCost 2
pairAccess = stepCost 1
cons = allocCost 2
loopCycle = stepCost 2
listSize = stepCost 2

emptySet, emptyMap :: Cost
emptySet = module_
emptyMap = module_

setMem :: Set k -> Cost
setMem = stepCost . treeAccess

mapMem :: Map k v -> Cost
mapMem = stepCost . treeAccess

bigMapMem :: Map k v -> Cost
bigMapMem _ = stepCost 50

mapGet :: Map k v -> Cost
mapGet = mapMem

bigMapGet :: Map k v -> Cost
bigMapGet _ = stepCost 50

setSize, mapSize :: Cost
setSize = stepCost 2
mapSize = stepCost 2

addSubZ :: Word64 -> Word64 -> Cost
addSubZ (Script.numbits -> n1) (Script.numbits -> n2) =
  stepCost bits <> allocCost bits
  where bits = max n1 n2

addTimestamp :: Timestamp -> Word64 -> Cost
addTimestamp (Script.timestampToZInt -> t1) t2 =
  addSubZ t1 t2

subTimestamp :: Timestamp -> Word64 -> Cost
subTimestamp = addTimestamp

diffTimestamps :: Timestamp -> Timestamp -> Cost
diffTimestamps (Script.timestampToZInt -> t1) (Script.timestampToZInt -> t2) =
  addSubZ t1 t2

concat_ :: (Foldable t, Monoid m) =>
                 (b -> m) -> (a -> b) -> t a -> m
concat_ cost len = Foldable.foldMap (cost . len)

concatString :: Foldable f => f Text -> Cost
concatString = concat_ string (fromIntegral . Text.length)

sliceString :: Word64 -> Cost
sliceString = string

concatBytes :: Foldable f => f BS.ByteString -> Cost
concatBytes = concat_ bytes (fromIntegral . BS.length)

int64Op :: Cost
int64Op = stepCost 1 <> allocCost 1

boolBinOp, boolUnOp :: Cost
boolBinOp = stepCost 1
boolUnOp = stepCost 1

abs :: Integer -> Cost
abs = allocBitsCost . Script.numbits . fromIntegral

int :: Cost
int = stepCost 1

neg :: Integer -> Cost
neg = abs

add, sub, mul, div :: Word64 -> Word64 -> Cost
add = addSubZ
sub = add
mul (Script.numbits -> n1) (Script.numbits -> n2) =
  stepCost (n1 * n2) <> allocCost (n1 + n2)
div n1 n2 = mul n1 n2 <> allocCost 2

int64ToZ :: Cost
int64ToZ = stepCost 2 <> allocCost 1

shiftLeft, shiftRight, bitwiseBinOp :: Word64 -> Word64 -> Cost
shiftLeft  (Script.numbits -> x) y = allocBitsCost (x + y)
shiftRight (Script.numbits -> x) y = allocBitsCost (max 1 (x - y))
bitwiseBinOp (Script.numbits -> n1) (Script.numbits -> n2) =
  let bits = max n1 n2
  in stepCost bits <> allocBitsCost bits

logOr, logAnd, logXor :: Word64 -> Word64 -> Cost
logOr = bitwiseBinOp
logAnd = bitwiseBinOp
logXor = bitwiseBinOp

logNot :: Word64 -> Cost
logNot (Script.numbits -> bits) = stepCost bits <> allocBitsCost bits

exec :: Cost
exec = stepCost 1

compareBool :: Cost
compareBool = stepCost 1

compareString :: Text -> Text -> Cost
compareString (Text.length -> s1) (Text.length -> s2) =
  stepCost (fromIntegral ((7 + max s1 s2) `Prelude.div` 8)) <> stepCost 1

compareBytes :: ByteString -> ByteString -> Cost
compareBytes (BS.length -> s1) (BS.length -> s2) =
  stepCost (fromIntegral ((7 + max s1 s2) `Prelude.div` 8)) <> stepCost 1

compareTez :: Cost
compareTez = stepCost 1

compareZInt :: Word64 -> Word64 -> Cost
compareZInt (Script.numbits -> n1) (Script.numbits -> n2) =
  stepCost ((7 + max n1 n2) `Prelude.div` 8) <> stepCost 1

compareInt :: Integer -> Integer -> Cost
compareInt x y = compareZInt (fromIntegral x) (fromIntegral y)

compareNat :: Natural -> Natural -> Cost
compareNat x y =
  compareZInt (fromIntegral . toInteger $ x) (fromIntegral . toInteger $ y)

compareKeyHash :: Cost
compareKeyHash = allocBytesCost 36

compareTimestamp :: Timestamp -> Timestamp -> Cost
compareTimestamp (Script.timestampToZInt -> t1) (Script.timestampToZInt -> t2) =
  compareZInt t1 t2

compareAddress :: Cost
compareAddress = stepCost 20

compareRes :: Cost
compareRes = stepCost 1

unpackFailed :: ByteString -> Cost
unpackFailed (BS.length -> len) =
  mconcat
    [ allocBytesCost (fromIntegral len)
    , scale (fromIntegral $ len * Script.log2 len) (allocCost 3 <> stepCost 1)
    ]

address :: Cost
address = stepCost 1

contract :: Cost
contract = readBytesCost 0 <> stepCost 10000

transfer, createAccount, createContract, implicitAccount :: Cost
transfer = stepCost 10
createAccount = stepCost 10
createContract = stepCost 10
implicitAccount = stepCost 10

setDelegate, balance, now, checkSignature :: Cost
setDelegate = stepCost 10 <> writeBytesCost 32
balance = stepCost 1 <> readBytesCost 8
now = stepCost 5
checkSignature = stepCost 1000

source, sender, self, amount, hashKey, stepsToQuota :: Cost
source = stepCost 1
sender = stepCost 1
self = stepCost 1
amount = stepCost 1
hashKey = stepCost 3 <> bytes 20
stepsToQuota = stepCost 1
