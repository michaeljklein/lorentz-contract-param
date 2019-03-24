module Lorentz.Value (
    Value
  , vC
  , vKey
  , vUnit
  , vSignature
  , vOption
  , vList
  , vSet
  , vOp
  , vContract
  , vPair
  , vOr
  , vLam
  , vMap
  , vBigMap
  , M.toVal
  , M.fromVal
  , CValue (..)
  , CVal
  , toCVal
  , fromCVal
  , Address
  , Mutez
  , Timestamp
  , KeyHash
  , PublicKey
  , Signature
  ) where

import Lorentz.Type
import Michelson.Typed.CValue (CVal, CValue(..), fromCVal, toCVal)
import qualified Michelson.Typed.Value as M
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

-- renaming because we want to use Val and CVal for type classes
type Value  = M.Value (:+>)

vC :: CValue t -> Value (Tc t)
vC = M.VC

vKey:: PublicKey -> Value TKey
vKey = M.VKey

vUnit  :: Value TUnit
vUnit = M.VUnit

vSignature :: Signature -> Value TSignature
vSignature = M.VSignature

vOption    :: Maybe (Value t) -> Value (TOption t)
vOption = M.VOption

vList  :: [Value t] -> Value (TList t)
vList = M.VList

vSet :: Set (CValue t) -> Value (TSet t)
vSet = M.VSet

vOp :: M.Operation (:+>) -> Value TOperation
vOp = M.VOp

vContract  :: Address -> Value (TContract p)
vContract  = M.VContract

vPair      :: (Value l, Value r) -> Value (TPair l r)
vPair = M.VPair

vOr :: Either (Value l) (Value r) -> Value (TOr l r)
vOr = M.VOr

vLam :: Show ('[ i ] :+> '[ o ]) => '[ i ] :+> '[ o ] -> Value (TLambda i o)
vLam = M.VLam

vMap :: Map (CValue k) (Value v) -> Value (TMap k v)
vMap = M.VMap

vBigMap :: Map (CValue k) (Value v) -> Value (TBigMap k v)
vBigMap = M.VBigMap

