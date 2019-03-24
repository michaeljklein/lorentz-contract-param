{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Michelson types represented in untyped model.

module Michelson.Untyped.Type
  ( Type (..)
  , Comparable (..)
  , compToType
  , typeToComp
  , T (..)
  , CT (..)
  , ToCT
  , FromCT
  , pattern TInt
  , pattern TNat
  , pattern TString
  , pattern TBytes
  , pattern TMutez
  , pattern TBool
  , pattern TKeyHash
  , pattern TTimestamp
  , pattern TAddress
  , CInt
  , CNat
  , CString
  , CBytes
  , CMutez
  , CBool
  , CKeyHash
  , CTimestamp
  , CAddress
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
  , isAtomicType
  , isKey
  , isSignature
  , isComparable
  , isMutez
  , isKeyHash
  , isBool
  , isString
  , isInteger
  , isTimestamp
  , isNat
  , isInt
  , isBytes
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Data.Text.Lazy.Builder (Builder)
import Fmt ((+|), (|+))
import Formatting.Buildable (Buildable(build))

import Michelson.Untyped.Annotation
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash)

-- Annotated type
data Type = Type T TypeAnn
  deriving (Eq, Show, Data, Generic)

instance Buildable Type where
  build (Type t a) = t |+ " " +| a |+ ""

-- Annotated Comparable Sub-type
data Comparable = Comparable CT TypeAnn
  deriving (Eq, Show, Data, Generic)

instance Buildable Comparable where
  build (Comparable ct a)
    | a == noAnn = build ct
    | otherwise = ct |+ " " +| a |+ ""

compToType :: Comparable -> Type
compToType (Comparable ct tn) = Type (Tc ct) tn

typeToComp :: Type -> Maybe Comparable
typeToComp (Type (Tc ct) tn) = Just $ Comparable ct tn
typeToComp _ = Nothing

-- Michelson Type
data T =
    Tc CT
  | TKey
  | TUnit
  | TSignature
  | TOption FieldAnn Type
  | TList Type
  | TSet Comparable
  | TOperation
  | TContract Type
  | TPair FieldAnn FieldAnn Type Type
  | TOr FieldAnn FieldAnn Type Type
  | TLambda Type Type
  | TMap Comparable Type
  | TBigMap Comparable Type
  deriving (Eq, Show, Data, Generic)

instance Buildable T where
  build =
    \case
      Tc ct -> build ct
      TKey -> "key"
      TUnit -> "unit"
      TSignature -> "signature"
      TOption fa t -> "option (" +| t |+ " " +| fa |+ ")"
      TList t -> "list (" +| t |+ ")"
      TSet c -> "set (" +| c |+ ")"
      TOperation -> "operation"
      TContract t -> "contract " +| t |+ ""
      TPair fa1 fa2 t1 t2 ->
        "pair (" +| t1 |+ " " +| fa1 |+ ")"
         +| " (" +| t2 |+ " " +| fa2 |+ ")"
      TOr fa1 fa2 t1 t2 ->
        "or ("   +| t1 |+ " " +| fa1 |+ ")"
         +| " (" +| t2 |+ " " +| fa2 |+ ")"
      TLambda t1 t2 -> build2 "lambda" t1 t2
      TMap t1 t2 -> build2 "map" t1 t2
      TBigMap t1 t2 -> build2 "big_map" t1 t2
    where
      -- build something with 2 type parameters
      build2 :: (Buildable t1, Buildable t2) => Builder -> t1 -> t2 -> Builder
      build2 name t1 t2 = name |+ " (" +| t1 |+ " " +| t2 |+ ")"

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
  ToCT Integer    = 'CInt
  ToCT Natural    = 'CNat
  ToCT Text       = 'CString
  ToCT Bool       = 'CBool
  ToCT ByteString = 'CBytes
  ToCT Mutez      = 'CMutez
  ToCT Address    = 'CAddress
  ToCT KeyHash    = 'CKeyHash
  ToCT Timestamp  = 'CTimestamp

type family FromCT (a :: CT) where
  FromCT 'CInt       = Integer
  FromCT 'CNat       = Natural
  FromCT 'CString    = Text
  FromCT 'CBool      = Bool
  FromCT 'CBytes     = ByteString
  FromCT 'CMutez     = Mutez
  FromCT 'CAddress   = Address
  FromCT 'CKeyHash   = KeyHash
  FromCT 'CTimestamp = Timestamp

instance Buildable CT where
  build =
    \case
      CInt -> "int"
      CNat -> "nat"
      CString -> "string"
      CBytes -> "bytes"
      CMutez -> "mutez"
      CBool -> "bool"
      CKeyHash -> "key_hash"
      CTimestamp -> "timestamp"
      CAddress -> "address"

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


isAtomicType :: Type -> Bool
isAtomicType t@(Type _ (Annotation "")) =
    isComparable t || isKey t || isUnit t || isSignature t || isOperation t
isAtomicType _ = False

isKey :: Type -> Bool
isKey (Type TKey _) = True
isKey _              = False

isUnit :: Type -> Bool
isUnit (Type TUnit _) = True
isUnit _               = False

isSignature :: Type -> Bool
isSignature (Type TSignature _) = True
isSignature _                    = False

isOperation :: Type -> Bool
isOperation (Type TOperation _) = True
isOperation _                    = False

isComparable :: Type -> Bool
isComparable (Type (Tc _) _) = True
isComparable _ = False

isMutez :: Type -> Bool
isMutez (Type TBool _) = True
isMutez _ = False

isTimestamp :: Type -> Bool
isTimestamp (Type TTimestamp _) = True
isTimestamp _ = False

isKeyHash :: Type -> Bool
isKeyHash (Type TKeyHash _) = True
isKeyHash _ = False

isBool  :: Type -> Bool
isBool (Type TBool _) = True
isBool _ = False

isString  :: Type -> Bool
isString (Type TString _) = True
isString _ = False

isInteger :: Type -> Bool
isInteger a = isNat a || isInt a || isMutez a || isTimestamp a

isNat  :: Type -> Bool
isNat (Type TNat _) = True
isNat _ = False

isInt  :: Type -> Bool
isInt (Type TInt _) = True
isInt _ = False

isBytes :: Type -> Bool
isBytes (Type TBytes _) = True
isBytes _ = False

----------------------------------------------------------------------------
-- JSON serialization
----------------------------------------------------------------------------

deriveJSON defaultOptions ''Type
deriveJSON defaultOptions ''Comparable
deriveJSON defaultOptions ''T
deriveJSON defaultOptions ''CT
