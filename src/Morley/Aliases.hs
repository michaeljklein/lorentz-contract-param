-- | Some simple aliases for Michelson types.

module Morley.Aliases
  ( UntypedContract
  , UntypedValue
  ) where

import qualified Michelson.Untyped as Untyped

type UntypedValue = Untyped.Value
type UntypedContract = Untyped.Contract
