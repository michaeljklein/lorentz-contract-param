module Lorentz.Type (
  CT (..)
  , T (..)
  , Contract
  , type ( & )
  , type (+>)
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
  ) where

import Michelson.Typed (CT(..), Contract, FromCT, FromT, Instr(..), T(..), ToCT, ToT)

-- infix syntax "stack function" syntax
type (+>) = Instr

-- Convert a Haskell type @*@ to a Michelson Type @T@, reexporting @ToT@ for
-- concision in type signatures

-- M for "Michelson type"
type M t = ToT t
-- H for "Haskell type"
type H t = FromT t

-- Mc for "Michelson (comparable) sub-type"
type Mc t = ToCT t
type Hc t = FromCT t

infixr 1 +>

-- Type-level Heterogenous stack cons
type (&) (a :: T) (b :: [T]) = a ': b

infixr 2 &

type Tc = 'T_c

type TInt       = 'T_c 'T_int
type TNat       = 'T_c 'T_nat
type TString    = 'T_c 'T_string
type TBytes     = 'T_c 'T_bytes
type TMutez     = 'T_c 'T_mutez
type TBool      = 'T_c 'T_bool
type TKeyHash   = 'T_c 'T_key_hash
type TTimestamp = 'T_c 'T_timestamp
type TAddress   = 'T_c 'T_address

type TKey       = 'T_key
type TUnit      = 'T_unit
type TSignature = 'T_signature
type TOption    = 'T_option
type TList      = 'T_list
type TSet       = 'T_set
type TOperation = 'T_operation
type TContract  = 'T_contract
type TPair      = 'T_pair
type TOr        = 'T_or
type TLambda    = 'T_lambda
type TMap       = 'T_map
type TBigMap    = 'T_big_map

