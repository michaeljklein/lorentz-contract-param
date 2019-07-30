{-# LANGUAGE DataKinds #-}

-- | Module, providing 'CT' and 'T' data types, representing Michelson
-- language types without annotations.
module Michelson.Typed.T
  ( CT (..)
  , T (..)
  , TFieldAnn(..)
  , EraseNotes
  ) where

import GHC.TypeLits
import Text.Show (Show(..))

import Michelson.Untyped.Type (CT(..))

data TFieldAnn = TFieldAnn Text | TFieldAnnS Symbol
-- | Wrapper to hold the field annotations. The `TFieldAnnS` constructor
-- is used to represent the annotation at the type level.

instance Show TFieldAnn where
  show (TFieldAnn s) = "TFieldAnn " ++ (toString s)
  show (TFieldAnnS _) = error "Impossible!" -- value of type symbol do not exist


instance Eq TFieldAnn where
  (TFieldAnn x) == (TFieldAnn y) = x == y
  _ == _ = error "Impossible"

-- | Michelson language type with annotations stripped off.
-- The `TFAnnotated` wrapper lets it have optional field annotations.
-- Thus, if there needs to be a field annotation for the `TOption`
-- field, It can be represented by wrapping the inner `T` again in the 
-- `TFAnnotated` constructor along with the required annotation.
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
  | TFAnnotated TFieldAnn T
  deriving (Eq, Show)

-- | Unwrap/Erase annotation information from the type.
type family EraseNotes (a :: T) :: T where
  EraseNotes ('TFAnnotated _ t) = EraseNotes t
  EraseNotes ('TOption t) = 'TOption (EraseNotes t)
  EraseNotes ('TList t) = 'TList (EraseNotes t)
  EraseNotes ('TContract t) = 'TContract (EraseNotes t)
  EraseNotes ('TPair t1 t2) = 'TPair (EraseNotes t1) (EraseNotes t2)
  EraseNotes ('TOr t1 t2) = 'TOr (EraseNotes t1) (EraseNotes t2)
  EraseNotes ('TLambda t1 t2) = 'TLambda (EraseNotes t1) (EraseNotes t2)
  EraseNotes ('TMap c t1) = 'TMap c (EraseNotes t1)
  EraseNotes ('TBigMap c t1) = 'TBigMap c (EraseNotes t1)
  EraseNotes t = t

