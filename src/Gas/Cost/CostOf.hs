
module Gas.Cost.CostOf where

import Prelude hiding (abs, div)
import qualified Prelude (div)

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Foldable as Foldable

import Gas.Cost
import Gas.Type
import qualified Gas.Script as Script

class HasSize b where
  sizeOf :: b -> Word64

zToInt64 :: Cost
zToInt64 = stepCost 2 <> allocCost 1

setAccess :: HasSize a => b -> a -> Word64
setAccess _key = Script.log2 . sizeOf

mapAccess :: HasSize a => b -> a -> Word64
mapAccess _key = Script.log2 . sizeOf

setUpdate key _presence set = setAccess key set `scale` allocCost 3
mapUpdate key _presence set = setAccess key set `scale` allocCost 3
bigMapUpdate _ _ _ = stepCost 10

hash data_ len = 10 `scale`
  stepCost (fromIntegral $ BS.length data_)
  <> bytes len

mapToList, setToList :: HasSize a => a -> Cost
mapToList = scale 3 . allocCost . sizeOf
setToList = scale 3 . allocCost . sizeOf

module_ = allocCost 10
cycle = stepCost 1
stackOp = stepCost 1
push = stepCost 1
wrap = allocCost 1
variantNoData = allocCost 1
branch = stepCost 2
pair = allocCost 2
pair_access = stepCost 1
cons = allocCost 2
loopCycle = stepCost 2
listSize = stepCost 2

emptySet = module_
emptyMap = module_

setMem key = stepCost . setAccess key
mapMem key = stepCost . setAccess key
bigMapMem _ _ = stepCost 50

mapGet :: HasSize a => b -> a -> Cost
mapGet = mapMem
bigMapGet _ _ = stepCost 50

setSize = stepCost 2
mapSize = stepCost 2

addSubZ (Script.numbits -> n1) (Script.numbits -> n2) =
  stepCost bits <> allocCost bits
  where bits = max n1 n2

addTimestamp (Script.timestampToZInt -> t1) t2 =
  addSubZ t1 t2

subTimestamp = addTimestamp

diffTimestamps (Script.timestampToZInt -> t1) (Script.timestampToZInt -> t2) =
  addSubZ t1 t2

concat_ cost len = Foldable.foldMap (cost . len)

concatString :: Foldable f => f Text -> Cost
concatString = concat_ string (fromIntegral . Text.length)

sliceString = string

concatBytes :: Foldable f => f BS.ByteString -> Cost
concatBytes = concat_ bytes (fromIntegral . BS.length)

int64Op = stepCost 1 <> allocCost 1

boolBinOp = stepCost 1
boolUnOp = stepCost 1

abs = allocBitsCost . Script.numbits . fromIntegral
int = stepCost 1
neg = abs
add = addSubZ
sub = add
mul (Script.numbits -> n1) (Script.numbits -> n2) =
  stepCost (n1 * n2) <> allocCost (n1 + n2)

div n1 n2 = mul n1 n2 <> allocCost 2

int64ToZ = stepCost 2 <> allocCost 1

shiftLeft  (Script.numbits -> x) y = allocBitsCost (x + y)
shiftRight (Script.numbits -> x) y = allocBitsCost (max 1 (x - y))

bitwiseBinOp (Script.numbits -> n1) (Script.numbits -> n2) =
  let bits = max n1 n2
  in stepCost bits <> allocBitsCost bits

logOr = bitwiseBinOp
logAnd = bitwiseBinOp
logXor = bitwiseBinOp
logNot = bitwiseBinOp 0

exec = stepCost 1

compareBool _ _ = stepCost 1

compareString (Text.length -> s1) (Text.length -> s2) =
  stepCost (fromIntegral ((7 + max s1 s2) `Prelude.div` 8)) <> stepCost 1

compareBytes (BS.length -> s1) (BS.length -> s2) =
  stepCost (fromIntegral ((7 + max s1 s2) `Prelude.div` 8)) <> stepCost 1

compareTez _ _ = stepCost 1

compareZInt (Script.numbits -> n1) (Script.numbits -> n2) =
  stepCost ((7 + max n1 n2) `Prelude.div` 8) <> stepCost 1

compareInt = compareZInt

compareNat = compareZInt

compareKeyHash = allocBytesCost 36

compareTimestamp (Script.timestampToZInt -> t1) (Script.timestampToZInt -> t2) =
  compareZInt t1 t2

compareAddress = stepCost 20

compareRes = stepCost 1

unpackFailed (BS.length -> len) =
  mconcat
    [ allocBytesCost (fromIntegral len)
    , scale (fromIntegral $ len * Script.log2 len) (allocCost 3 <> stepCost 1)
    ]

address = stepCost 1

contract = readBytesCost 0 <> stepCost 10000

transfer = stepCost 10
createAccount = stepCost 10
createContract = stepCost 10
implicitAccount = stepCost 10
setDelegate = stepCost 10 <> writeBytesCost 32
balance = stepCost 1 <> readBytesCost 8
now = stepCost 5
checkSignature = stepCost 1000
source = stepCost 1
self = stepCost 1
amount = stepCost 1
