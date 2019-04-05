module Michelson.TypeCheck
  ( typeCheckContract
  , typeCheckVal
  , typeCheckOp
  , typeCheckCVal
  , module M
  , eqT'
  ) where

import Michelson.TypeCheck.Instr
import Michelson.TypeCheck.Types as M
import Michelson.TypeCheck.Value

import Michelson.TypeCheck.Helpers (eqT')
