{-# LANGUAGE DerivingStrategies #-}

-- | Conversions between haskell types/values and Michelson ones.
module Michelson.Typed.Haskell.Value
  ( IsoValue (..)
  ) where

import qualified Data.Kind as Kind
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics ((:*:)(..), (:+:)(..))
import qualified GHC.Generics as G
import Named (NamedF(..))

import Michelson.Typed.Aliases
import Michelson.Typed.CValue
import Michelson.Typed.T
import Michelson.Typed.Value
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

-- | Isomorphism between Michelson values and plain Haskell types.
--
-- Default implementation of this typeclass converts ADTs to Michelson
-- "pair"s and "or"s.
class IsoValue a where
  -- | Type function that converts a regular Haskell type into a @T@ type.
  type ToT a :: T

  -- | Converts a Haskell structure into @Value@ representation.
  toVal :: a -> Value (ToT a)

instance IsoValue Integer where
  type ToT Integer = 'Tc (ToCT Integer)
  toVal = VC . toCVal
