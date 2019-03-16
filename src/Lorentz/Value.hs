module Lorentz.Value (
    type Value
  , Val (..)
  , type CValue
  , CVal (..)
  ) where

import Lorentz.Type
import Michelson.Typed.CValue (FromCVal(..), ToCVal(..))
import qualified Michelson.Typed.CValue as M
import Michelson.Typed.Value (FromVal(..), ToVal(..))
import qualified Michelson.Typed.Value as M
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash)

-- renaming because we want to use Val and CVal for type classes
type Value  = M.Val (+>)
type CValue = M.CVal

class (FromVal a, ToVal a) => Val a where
  toVal   :: a -> Value (M a)
  fromVal :: Value (M a) -> a

class (Ord a, ToCVal a, FromCVal a) => CVal a where
  toCVal   :: a -> CValue (Mc a)
  fromCVal :: CValue (Mc a) -> a

instance CVal Integer where
  toCVal   = M.toCVal
  fromCVal = M.fromCVal
instance CVal Natural where
  fromCVal = M.fromCVal
  toCVal   = M.toCVal

instance CVal Text where
  fromCVal = M.fromCVal
  toCVal   = M.toCVal

instance CVal Bool where
  fromCVal = M.fromCVal
  toCVal   = M.toCVal

instance CVal ByteString where
  fromCVal = M.fromCVal
  toCVal   = M.toCVal

instance CVal Mutez where
  fromCVal = M.fromCVal
  toCVal   = M.toCVal

instance CVal KeyHash where
  fromCVal = M.fromCVal
  toCVal   = M.toCVal

instance CVal Timestamp where
  fromCVal = M.fromCVal
  toCVal   = M.toCVal

instance CVal Address where
  fromCVal = M.fromCVal
  toCVal   = M.toCVal

instance Val Integer where
  fromVal = M.fromVal
  toVal   = M.toVal

instance Val Natural where
  fromVal = M.fromVal
  toVal   = M.toVal

instance Val Text where
  fromVal = M.fromVal
  toVal   = M.toVal

instance Val ByteString where
  fromVal = M.fromVal
  toVal   = M.toVal

instance Val Bool where
  fromVal = M.fromVal
  toVal   = M.toVal

instance Val Mutez where
  fromVal = M.fromVal
  toVal   = M.toVal

instance Val KeyHash where
  fromVal = M.fromVal
  toVal   = M.toVal

instance Val Timestamp where
  fromVal = M.fromVal
  toVal   = M.toVal

instance Val Address where
  fromVal = M.fromVal
  toVal   = M.toVal

instance Val () where
  fromVal = M.fromVal
  toVal   = M.toVal

instance Val a => Val (Maybe a) where
  fromVal = M.fromVal
  toVal   = M.toVal

instance (Val a, Val b) => Val (Either a b) where
  fromVal = M.fromVal
  toVal   = M.toVal

instance (Val a, Val b) => Val (a, b) where
  fromVal = M.fromVal
  toVal   = M.toVal

instance Val a => Val [a] where
  fromVal = M.fromVal
  toVal   = M.toVal

instance CVal k => Val (Set k) where
  fromVal = M.fromVal
  toVal   = M.toVal

instance (CVal k, Val a) => Val (Map k a) where
  fromVal = M.fromVal
  toVal   = M.toVal
