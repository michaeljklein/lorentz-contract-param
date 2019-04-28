-- | Some simple aliases for Michelson types.

module Michelson.Untyped.Aliases
  ( Value
  , ExpandedExtInstr
  ) where

import qualified Michelson.Untyped.Ext as Untyped
import qualified Michelson.Untyped.Instr as Untyped
import qualified Michelson.Untyped.Value as Untyped

type Value = Untyped.Value' Untyped.ExpandedOp
type ExpandedExtInstr = Untyped.ExtInstrAbstract Untyped.ExpandedOp
