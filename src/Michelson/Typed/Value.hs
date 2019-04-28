-- | Module, containing data types for Michelson value.

module Michelson.Typed.Value
  ( Value' (..)
  , CValue (..)
  , IsoValue (..)
  ) where

import Universum

import Michelson.Typed.CValue (CValue(..), toCVal)
import Michelson.Typed.T

data Value' t where
  VC :: CValue t -> Value' ('Tc t)

deriving instance Show (Value' t)
deriving instance Eq (Value' t)

class IsoValue a where
  type ToT a :: T
  toVal :: a -> Value' (ToT a)

instance IsoValue Integer where
  type ToT Integer = 'Tc (ToCT Integer)
  toVal = VC . toCVal
