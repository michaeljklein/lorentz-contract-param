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
  , pattern Tint
  , pattern Tnat
  , pattern Tstring
  , pattern Tbytes
  , pattern Tmutez
  , pattern Tbool
  , pattern Tkey_hash
  , pattern Ttimestamp
  , pattern Taddress
  , tint
  , tnat
  , tstring
  , tbytes
  , tmutez
  , tbool
  , tkeyHash
  , ttimestamp
  , taddress
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

import Michelson.Untyped.Annotation (Annotation(..), FieldAnn, TypeAnn)
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash)

-- Annotated type
data Type = Type T TypeAnn
  deriving (Eq, Show, Data, Generic)

-- Annotated Comparable Sub-type
data Comparable = Comparable CT TypeAnn
  deriving (Eq, Show, Data, Generic)

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

pattern Tint :: T
pattern Tint <- Tc CInt

pattern Tnat :: T
pattern Tnat <- Tc CNat

pattern Tstring :: T
pattern Tstring <- Tc CString

pattern Tbytes :: T
pattern Tbytes <- Tc CBytes

pattern Tmutez :: T
pattern Tmutez <- Tc CMutez

pattern Tbool :: T
pattern Tbool <- Tc CBool

pattern Tkey_hash :: T
pattern Tkey_hash <- Tc CKeyHash

pattern Ttimestamp :: T
pattern Ttimestamp <- Tc CTimestamp

pattern Taddress :: T
pattern Taddress <- Tc CAddress

tint :: T
tint = Tc CInt

tnat :: T
tnat = Tc CNat

tstring :: T
tstring = Tc CString

tbytes :: T
tbytes = Tc CBytes

tmutez :: T
tmutez = Tc CMutez

tbool :: T
tbool = Tc CBool

tkeyHash :: T
tkeyHash = Tc CKeyHash

ttimestamp :: T
ttimestamp = Tc CTimestamp

taddress :: T
taddress = Tc CAddress

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
isMutez (Type (Tc CMutez) _) = True
isMutez _ = False

isTimestamp :: Type -> Bool
isTimestamp (Type (Tc CTimestamp) _) = True
isTimestamp _ = False

isKeyHash :: Type -> Bool
isKeyHash (Type (Tc CKeyHash) _) = True
isKeyHash _ = False

isBool  :: Type -> Bool
isBool (Type (Tc CBool) _) = True
isBool _ = False

isString  :: Type -> Bool
isString (Type (Tc CString) _) = True
isString _ = False

isInteger :: Type -> Bool
isInteger a = isNat a || isInt a || isMutez a || isTimestamp a

isNat  :: Type -> Bool
isNat (Type (Tc CNat) _) = True
isNat _ = False

isInt  :: Type -> Bool
isInt (Type (Tc CInt) _) = True
isInt _ = False

isBytes :: Type -> Bool
isBytes (Type (Tc CBytes) _) = True
isBytes _ = False
