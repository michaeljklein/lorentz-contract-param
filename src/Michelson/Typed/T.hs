{-# LANGUAGE DataKinds #-}

-- | Module, providing 'CT' and 'T' data types, representing Michelson
-- language types without annotations.
module Michelson.Typed.T
  ( CT (..)
  , T (..)
  , ToCT
  , FromCT
  , ToT
  , FromT
  , pattern TInt
  , pattern TNat
  , pattern TString
  , pattern TBytes
  , pattern TMutez
  , pattern TBool
  , pattern TKeyHash
  , pattern TTimestamp
  , pattern TAddress
  , Tc
  , CInt
  , CNat
  , CString
  , CBytes
  , CMutez
  , CBool
  , CKeyHash
  , CTimestamp
  , CAddress
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

import Michelson.Untyped.Type (CT(..), FromCT, ToCT)
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

-- | Michelson language type with annotations stripped off.
data T =
    Tc CT
  | TKey
  | TUnit
  | TSignature
  | TOption T
  | TList T
  | TSet CT
  | TOperation
  | TContract T
  | TPair T T
  | TOr T T
  | TLambda T T
  | TMap CT T
  | TBigMap CT T
  deriving (Eq, Show)

-- | Type function that converts a regular Haskell type into a @T@ type.
-- TODO: what should be done with 'T_big_map'?
type family ToT t :: T where
  ToT Integer      = Tc (ToCT Integer)
  ToT Natural      = Tc (ToCT Natural)
  ToT Text         = Tc (ToCT Text)
  ToT Bool         = Tc (ToCT Bool)
  ToT ByteString   = Tc (ToCT ByteString)
  ToT Mutez        = Tc (ToCT Mutez)
  ToT Address      = Tc (ToCT Address)
  ToT KeyHash      = Tc (ToCT KeyHash)
  ToT Timestamp    = Tc (ToCT Timestamp)

  ToT ()           = TUnit
  ToT (a, b)       = TPair (ToT a) (ToT b)
  ToT [a]          = TList (ToT a)
  ToT (Maybe a)    = TOption (ToT a)
  ToT (Either a b) = TOr (ToT a) (ToT b)
  ToT (Set k)      = TSet (ToCT k)
  ToT (Map k v)    = TMap (ToCT k) (ToT v)
  ToT PublicKey    = TKey
  ToT Signature    = TSignature

type family FromT (t :: T) where
  FromT (Tc a)      = FromCT a
  FromT TUnit       = ()
  FromT (TPair a b) = (FromT a, FromT b)
  FromT (TList a)   = [FromT a]
  FromT (TOption a) = Maybe (FromT a)
  FromT (TOr a b)   = Either (FromT a) (FromT b)
  FromT (TSet k)    = Set (FromCT k)
  FromT (TMap k v)  = Map (FromCT k) (FromT v)
  FromT TKey        = PublicKey
  FromT TSignature  = Signature

type Tc = 'Tc

type CInt       = 'CInt
type CNat       = 'CNat
type CString    = 'CString
type CBytes     = 'CBytes
type CMutez     = 'CMutez
type CBool      = 'CBool
type CKeyHash   = 'CKeyHash
type CTimestamp = 'CTimestamp
type CAddress   = 'CAddress

type TInt       = 'Tc 'CInt
type TNat       = 'Tc 'CNat
type TString    = 'Tc 'CString
type TBytes     = 'Tc 'CBytes
type TMutez     = 'Tc 'CMutez
type TBool      = 'Tc 'CBool
type TKeyHash   = 'Tc 'CKeyHash
type TTimestamp = 'Tc 'CTimestamp
type TAddress   = 'Tc 'CAddress

type TKey       = 'TKey
type TUnit      = 'TUnit
type TSignature = 'TSignature
type TOption    = 'TOption
type TList      = 'TList
type TSet       = 'TSet
type TOperation = 'TOperation
type TContract  = 'TContract
type TPair      = 'TPair
type TOr        = 'TOr
type TLambda    = 'TLambda
type TMap       = 'TMap
type TBigMap    = 'TBigMap

pattern TInt :: T
pattern TInt = Tc CInt

pattern TNat :: T
pattern TNat = Tc CNat

pattern TString :: T
pattern TString = Tc CString

pattern TBytes :: T
pattern TBytes = Tc CBytes

pattern TMutez :: T
pattern TMutez = Tc CMutez

pattern TBool :: T
pattern TBool = Tc CBool

pattern TKeyHash :: T
pattern TKeyHash = Tc CKeyHash

pattern TTimestamp :: T
pattern TTimestamp = Tc CTimestamp

pattern TAddress :: T
pattern TAddress = Tc CAddress
