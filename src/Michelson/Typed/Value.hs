-- | Module, containing data types for Michelson value.

module Michelson.Typed.Value
  ( Value' (..)
  , CValue (..)
  , IsoValue (..)
  , Instr (..)
  ) where

import Michelson.Typed.CValue (CValue(..), toCVal)
import Michelson.Typed.T

-- | Representation of Michelson value.
--
-- Type parameter @instr@ stands for Michelson instruction
-- type, i.e. data type to represent an instruction of language.

data Value' instr t where
  VC :: CValue t -> Value' instr ('Tc t)
  VUnit :: Value' instr 'TUnit
  VOption :: forall t instr. Maybe (Value' instr t) -> Value' instr ('TOption t)
  VList :: forall t instr. [Value' instr t] -> Value' instr ('TList t)
  VSet :: forall t instr. Set (CValue t) -> Value' instr ('TSet t)
  VPair :: forall l r instr. (Value' instr l, Value' instr r) -> Value' instr ('TPair l r)
  VOr :: forall l r instr. Either (Value' instr l) (Value' instr r) -> Value' instr ('TOr l r)
  VLam
    :: forall inp out instr.
       ( Show (instr '[inp] '[out])
       , Eq (instr '[inp] '[out])
       )
    => instr (inp ': '[]) (out ': '[]) -> Value' instr ('TLambda inp out)
  VMap :: forall k v instr. Map (CValue k) (Value' instr v) -> Value' instr ('TMap k v)
  VBigMap :: forall k v instr. Map (CValue k) (Value' instr v) -> Value' instr ('TBigMap k v)

deriving instance Show (Value' instr t)
deriving instance Eq (Value' instr t)

class IsoValue a where
  -- | Type function that converts a regular Haskell type into a @T@ type.
  type ToT a :: T

  -- | Converts a Haskell structure into @Value@ representation.
  toVal :: a -> Value' instr (ToT a)

instance IsoValue Integer where
  type ToT Integer = 'Tc (ToCT Integer)
  toVal = VC . toCVal

data Instr (inp :: [T]) (out :: [T]) where
  Seq :: Instr a b -> Instr b c -> Instr a c
