module Lorentz.Type (
  CT (..)
  , T
  , Contract
  , type ( & )
  , ( # )
  , (:+>)
  , M
  , Mc
  , H
  , Hc
  , Tc
  , TInt
  , TNat
  , TString
  , TBytes
  , TMutez
  , TBool
  , TKeyHash
  , TTimestamp
  , TAddress
  , TKey
  , TUnit
  , TSignature
  , TOption
  , TList
  , TSet
  , TOperation
  , TContract
  , TPair
  , TOr
  , TLambda
  , TMap
  , TBigMap
  , pattern TInt
  , pattern TNat
  , pattern TString
  , pattern TBytes
  , pattern TMutez
  , pattern TBool
  , pattern TKeyHash
  , pattern TTimestamp
  , pattern TAddress
  ) where

import Michelson.Typed

-- Convert a Haskell type @*@ to a Michelson Type @T@, reexporting @ToT@ for
-- concision in type signatures

-- M for "Michelson type"
type M t = ToT t
-- H for "Haskell type"
type H t = FromT t

-- Mc for "Michelson (comparable) sub-type"
type Mc t = ToCT t
type Hc t = FromCT t
