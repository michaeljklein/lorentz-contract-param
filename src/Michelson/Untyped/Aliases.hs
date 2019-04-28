{-
 - © 2019 Tocqueville Group
 -
 - SPDX-License-Identifier: AGPL-3.0-or-later
 -}

-- | Some simple aliases for Michelson types.

module Michelson.Untyped.Aliases
  ( Contract
  , Value
  ) where

import qualified Michelson.Untyped.Contract as Untyped
import qualified Michelson.Untyped.Instr as Untyped
import qualified Michelson.Untyped.Value as Untyped

type Value = Untyped.Value' Untyped.ExpandedOp
type Contract = Untyped.Contract' Untyped.ExpandedOp
