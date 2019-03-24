module Lorentz.Value (
    Value
  , M.Value (..)
  , Val
  , toValue
  , fromValue
  , CValue (..)
  , CVal
  , toCValue
  , fromCValue
  , Address
  , Mutez
  , Timestamp
  , KeyHash
  , PublicKey
  , Signature
  ) where

import Lorentz.Type
import Michelson.Typed.CValue (CVal, CValue(..))
import qualified Michelson.Typed.Value as M
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

-- renaming because we want to use Val and CVal for type classes
type Value  = M.Value (:+>)
