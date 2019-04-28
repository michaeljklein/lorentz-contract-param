{-
 - © 2019 Tocqueville Group
 -
 - SPDX-License-Identifier: AGPL-3.0-or-later
 -}

module Michelson.TypeCheck
  ( typeCheckContract
  , typeCheckValue
  , typeCheckList
  , typeCheckCValue
  , module M
  , eqT'
  ) where

import Michelson.TypeCheck.Instr
import Michelson.TypeCheck.Types as M
import Michelson.TypeCheck.Value

import Michelson.TypeCheck.Helpers (eqT')
