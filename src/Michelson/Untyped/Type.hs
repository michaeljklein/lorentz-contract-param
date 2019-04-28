{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Michelson types represented in untyped model.

module Michelson.Untyped.Type
  ( CT (..)
  , ToCT
  ) where

import Data.Data (Data(..))

import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash)

-- Comparable Sub-Type
data CT =
    CInt
  | CNat
  | CString
  | CBytes
  | CMutez
  | CBool
  | CKeyHash
  | CTimestamp
  | CAddress
  deriving (Eq, Ord, Show, Data, Enum, Bounded, Generic)

-- | Type function that converts a regular Haskell type into a comparable type
-- (which has kind @CT@)
type family ToCT a :: CT where
  ToCT Integer = 'CInt
  ToCT Int = 'CInt
  ToCT Natural = 'CNat
  ToCT Word64 = 'CNat
  ToCT Text = 'CString
  ToCT Bool = 'CBool
  ToCT ByteString = 'CBytes
  ToCT Mutez = 'CMutez
  ToCT Address = 'CAddress
  ToCT KeyHash = 'CKeyHash
  ToCT Timestamp = 'CTimestamp
