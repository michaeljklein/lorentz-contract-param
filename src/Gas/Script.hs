
module Gas.Script where

import Data.Bits (finiteBitSize, countLeadingZeros, Bits)
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text

import Gas.Type
import qualified Michelson.Untyped as U
import Tezos.Core

costOfSize :: (Word64, Word64) -> Cost
costOfSize (blocks, wordz) =
  scale (0 `max` blocks - 1) (allocCost 0)
  <> allocCost wordz
  <> stepCost blocks

intNodeSizeOfNumbits :: Int -> (Word64, Word64)
intNodeSizeOfNumbits (fromIntegral -> n) = (1, 1 + (n + 63) `div` 64)

intNodeSize :: Int -> (Word64, Word64)
intNodeSize
  = intNodeSizeOfNumbits
  . significantBitCount

stringNodeSizeOfLength :: Int -> (Word64, Word64)
stringNodeSizeOfLength (fromIntegral -> n) = (1, 1 + (n + 7) `div` 8)

stringNodeSize :: Text -> (Word64, Word64)
stringNodeSize
  = stringNodeSizeOfLength
  . Text.length

bytesNodeSizeOfLength :: Int -> (Word64, Word64)
bytesNodeSizeOfLength (fromIntegral -> n) = (2, 1 + (n + 7) `div` 8 + 12)

bytesNodeSize :: U.InternalByteString -> (Word64, Word64)
bytesNodeSize
  = bytesNodeSizeOfLength
  . BS.length
  . U.unInternalByteString

primNodeSizeNonrecOfLength :: (Functor f, Foldable f) => Int -> f Text -> (Word64, Word64)
primNodeSizeNonrecOfLength (fromIntegral -> nArgs) annots =
  case annotsLength of
    0 -> (1 + nArgs, 2 + 2 * nArgs)
    n -> (2 + nArgs, 4 + 2 * nArgs + (n + 7) `div` 8)
  where
    annotsLength = fromIntegral $ Foldable.sum $ fmap Text.length annots

primNodeSizeNonrec :: (Foldable f1, Functor f2, Foldable f2) => f1 a -> f2 Text -> (Word64, Word64)
primNodeSizeNonrec args annots = primNodeSizeNonrecOfLength (Foldable.length args) annots

seqNodeSizeNonrecOfLength :: Int -> (Word64, Word64)
seqNodeSizeNonrecOfLength (fromIntegral -> nArgs) = (1 + nArgs, 2 + 2 * nArgs)

seqNodeSizeNonrec :: Foldable f => f a -> (Word64, Word64)
seqNodeSizeNonrec = seqNodeSizeNonrecOfLength . Foldable.length

intNodeCost    :: Int -> Cost
stringNodeCost :: Text -> Cost
bytesNodeCost  :: U.InternalByteString -> Cost

stringNodeCostOfLength :: Int -> Cost
intNodeCostOfNumbits   :: Int -> Cost
bytesNodeCostOfLength  :: Int -> Cost

intNodeCost            = costOfSize . intNodeSize
stringNodeCost         = costOfSize . stringNodeSize
bytesNodeCost          = costOfSize . bytesNodeSize
intNodeCostOfNumbits   = costOfSize . intNodeSizeOfNumbits
stringNodeCostOfLength = costOfSize . stringNodeSizeOfLength
bytesNodeCostOfLength  = costOfSize . bytesNodeSizeOfLength

primNodeCostNonrec :: (Foldable f1, Functor f2, Foldable f2) => f1 a -> f2 Text -> Cost
primNodeCostNonrec args annots = costOfSize $ primNodeSizeNonrec args annots

primNodeCostNonrecOfLength :: (Functor f, Foldable f) => Int -> f Text -> Cost
primNodeCostNonrecOfLength nArgs annots = costOfSize $ primNodeSizeNonrecOfLength nArgs annots

seqNodeCostNonrec :: (Foldable f) => f a -> Cost
seqNodeCostNonrec = costOfSize . seqNodeSizeNonrec

seqNodeCostNonrecOfLength :: Int -> Cost
seqNodeCostNonrecOfLength = costOfSize . seqNodeSizeNonrecOfLength

significantBitCount ::  (Integral i, Bits i) => i -> i
significantBitCount = go
  where
    go 0 = 0
    go n = 1 + go (n `div` 2)

numbits :: (Integral i, Bits i) => i -> i
numbits = significantBitCount

log2 :: (Integral i, Bits i) => i -> i
log2 = (+ 1) . significantBitCount

timestampToZInt :: Timestamp -> Word64
timestampToZInt = error "Find what `timestamp` type is"
