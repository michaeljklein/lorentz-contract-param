module Lorentz.Type (
  CT (..)
  , T (..)
  , Contract
  , type ( & )
  , type (+>)
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


import Michelson.Typed (CT(..), Contract, Instr(..), T(..))

type (+>) = Instr

infixr 1 +>

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

