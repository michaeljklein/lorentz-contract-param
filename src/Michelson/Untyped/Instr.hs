{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Michelson instructions in untyped model.

module Michelson.Untyped.Instr
  ( InstrAbstract (..)
  , Op (..)
  , Instr
  , ExpandedOp (..)
  , ExpandedInstr
  ) where

import Data.Data (Data(..))

import Michelson.Untyped.Annotation (FieldAnn, TypeAnn, VarAnn)
import Michelson.Untyped.Ext (ExtInstrAbstract)
import Michelson.Untyped.Type (Comparable, Type)
import Michelson.Untyped.Value (Value'(..))
import Tezos.Core (Mutez)
import Tezos.Crypto (KeyHash)

-------------------------------------
-- Flattened types after macroexpander
-------------------------------------
type Instr = InstrAbstract Op
newtype Op = Op {unOp :: Instr}
  deriving stock (Show, Eq, Generic)

-------------------------------------
-- Types after macroexpander
-------------------------------------

type ExpandedInstr = InstrAbstract ExpandedOp

data ExpandedOp
  = PrimEx ExpandedInstr
  | SeqEx [ExpandedOp]
  deriving stock (Show, Eq, Data, Generic)

-------------------------------------
-- Abstract instruction
-------------------------------------

-- | Michelson instruction with abstract parameter `op`.  This
-- parameter is necessary, because at different stages of our pipeline
-- it will be different. Initially it can contain macros and
-- non-flattened instructions, but then it contains only vanilla
-- Michelson instructions.
data InstrAbstract op
  = DROP
  deriving (Eq, Show, Functor, Data, Generic)
