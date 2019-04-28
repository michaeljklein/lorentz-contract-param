module Michelson.Typed.T
  ( CT (..)
  , T (..)
  , ToCT
  ) where

import Universum

data CT =
    CInt
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

type family ToCT a :: CT where
  ToCT Integer = 'CInt

data T =
  Tc CT
  deriving (Eq, Show)
