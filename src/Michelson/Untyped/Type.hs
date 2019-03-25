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
compToType (Comparable ct tn) = Type (T_comparable ct) tn

typeToComp :: Type -> Maybe Comparable
typeToComp (Type (T_comparable ct) tn) = Just $ Comparable ct tn
typeToComp _ = Nothing

-- Michelson Type
data T =
    T_comparable CT
  | T_key
  | T_unit
  | T_signature
  | T_option FieldAnn Type
  | T_list Type
  | T_set Comparable
  | T_operation
  | T_contract Type
  | T_pair FieldAnn FieldAnn Type Type
  | T_or FieldAnn FieldAnn Type Type
  | T_lambda Type Type
  | T_map Comparable Type
  | T_big_map Comparable Type
  deriving (Eq, Show, Data, Generic)

instance Buildable T where
  build =
    \case
      T_comparable ct -> build ct
      T_key -> "key"
      T_unit -> "unit"
      T_signature -> "signature"
      T_option fa t -> "option (" +| t |+ " " +| fa |+ ")"
      T_list t -> "list (" +| t |+ ")"
      T_set c -> "set (" +| c |+ ")"
      T_operation -> "operation"
      T_contract t -> "contract " +| t |+ ""
      T_pair fa1 fa2 t1 t2 ->
        "pair (" +| t1 |+ " " +| fa1 |+ ")"
         +| " (" +| t2 |+ " " +| fa2 |+ ")"
      T_or fa1 fa2 t1 t2 ->
        "or ("   +| t1 |+ " " +| fa1 |+ ")"
         +| " (" +| t2 |+ " " +| fa2 |+ ")"
      T_lambda t1 t2 -> build2 "lambda" t1 t2
      T_map t1 t2 -> build2 "map" t1 t2
      T_big_map t1 t2 -> build2 "big_map" t1 t2
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

pattern Tint :: T
pattern Tint <- T_comparable CInt

pattern Tnat :: T
pattern Tnat <- T_comparable CNat

pattern Tstring :: T
pattern Tstring <- T_comparable CString

pattern Tbytes :: T
pattern Tbytes <- T_comparable CBytes

pattern Tmutez :: T
pattern Tmutez <- T_comparable CMutez

pattern Tbool :: T
pattern Tbool <- T_comparable CBool

pattern Tkey_hash :: T
pattern Tkey_hash <- T_comparable CKeyHash

pattern Ttimestamp :: T
pattern Ttimestamp <- T_comparable CTimestamp

pattern Taddress :: T
pattern Taddress <- T_comparable CAddress

tint :: T
tint = T_comparable CInt

tnat :: T
tnat = T_comparable CNat

tstring :: T
tstring = T_comparable CString

tbytes :: T
tbytes = T_comparable CBytes

tmutez :: T
tmutez = T_comparable CMutez

tbool :: T
tbool = T_comparable CBool

tkeyHash :: T
tkeyHash = T_comparable CKeyHash

ttimestamp :: T
ttimestamp = T_comparable CTimestamp

taddress :: T
taddress = T_comparable CAddress

isAtomicType :: Type -> Bool
isAtomicType t@(Type _ (Annotation "")) =
    isComparable t || isKey t || isUnit t || isSignature t || isOperation t
isAtomicType _ = False

isKey :: Type -> Bool
isKey (Type T_key _) = True
isKey _              = False

isUnit :: Type -> Bool
isUnit (Type T_unit _) = True
isUnit _               = False

isSignature :: Type -> Bool
isSignature (Type T_signature _) = True
isSignature _                    = False

isOperation :: Type -> Bool
isOperation (Type T_operation _) = True
isOperation _                    = False

isComparable :: Type -> Bool
isComparable (Type (T_comparable _) _) = True
isComparable _ = False

isMutez :: Type -> Bool
isMutez (Type (T_comparable CMutez) _) = True
isMutez _ = False

isTimestamp :: Type -> Bool
isTimestamp (Type (T_comparable CTimestamp) _) = True
isTimestamp _ = False

isKeyHash :: Type -> Bool
isKeyHash (Type (T_comparable CKeyHash) _) = True
isKeyHash _ = False

isBool  :: Type -> Bool
isBool (Type (T_comparable CBool) _) = True
isBool _ = False

isString  :: Type -> Bool
isString (Type (T_comparable CString) _) = True
isString _ = False

isInteger :: Type -> Bool
isInteger a = isNat a || isInt a || isMutez a || isTimestamp a

isNat  :: Type -> Bool
isNat (Type (T_comparable CNat) _) = True
isNat _ = False

isInt  :: Type -> Bool
isInt (Type (T_comparable CInt) _) = True
isInt _ = False

isBytes :: Type -> Bool
isBytes (Type (T_comparable CBytes) _) = True
isBytes _ = False

----------------------------------------------------------------------------
-- JSON serialization
----------------------------------------------------------------------------

deriveJSON defaultOptions ''Type
deriveJSON defaultOptions ''Comparable
deriveJSON defaultOptions ''T
deriveJSON defaultOptions ''CT
