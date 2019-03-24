-- | Module, containing CVal data type
-- which represents Michelson comparable values.

module Michelson.Typed.CValue
  ( CValue (..)
  , CVal
  , toCVal
  , fromCVal
  ) where

import Michelson.Typed.T
  (CAddress, CBool, CBytes, CInt, CKeyHash, CMutez, CNat, CString, CTimestamp, ToCT)
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash)

-- | Representation of comparable value
-- in Michelson language.
--
-- By specification, we're allowed to compare
-- only following types: int, nat, string, bytes,
-- mutez, bool, key_hash, timestamp, address.
--
-- Only these values can be used as map keys
-- or set elements.
data CValue t where
  CvInt       :: Integer -> CValue CInt
  CvNat       :: Natural -> CValue CNat
  CvString    :: Text -> CValue CString
  CvBytes     :: ByteString -> CValue CBytes
  CvMutez     :: Mutez -> CValue CMutez
  CvBool      :: Bool -> CValue CBool
  CvKeyHash   :: KeyHash -> CValue CKeyHash
  CvTimestamp :: Timestamp -> CValue CTimestamp
  CvAddress   :: Address -> CValue CAddress

deriving instance Show (CValue t)
deriving instance Eq (CValue t)
deriving instance Ord (CValue t)

-- | Converts a single Haskell value into @CVal@ representation and back
class Ord a => CVal a where
  toCVal   :: a -> CValue (ToCT a)
  fromCVal :: CValue (ToCT a) -> a

-- CVal instances
instance CVal Integer where
  toCVal = CvInt
  fromCVal (CvInt i) = i

instance CVal Natural where
  toCVal = CvNat
  fromCVal (CvNat i) = i

instance CVal Text where
  toCVal = CvString
  fromCVal (CvString s) = s

instance CVal Bool where
  toCVal = CvBool
  fromCVal (CvBool b) = b

instance CVal ByteString where
  toCVal = CvBytes
  fromCVal (CvBytes b) = b

instance CVal Mutez where
  toCVal = CvMutez
  fromCVal (CvMutez m) = m

instance CVal KeyHash where
  toCVal = CvKeyHash
  fromCVal (CvKeyHash k) = k

instance CVal Timestamp where
  toCVal = CvTimestamp
  fromCVal (CvTimestamp t) = t

instance CVal Address where
  toCVal = CvAddress
  fromCVal (CvAddress a) = a



