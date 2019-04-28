{-# LANGUAGE DerivingStrategies #-}

-- | Module, containing data types for Michelson value.

module Michelson.Typed.Instr
  ( Instr (..)
  ) where

import Data.Singletons (SingI)
import Data.Singletons (sing)
import Fmt ((+||), (||+))
import qualified Text.Show

import Michelson.Typed.Annotation (Notes)
import Michelson.Typed.Arith
import Michelson.Typed.Scope
import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.Value (ContractInp, ContractOut, Value'(..))
import Util.Peano

-- | Representation of Michelson instruction or sequence
-- of instructions.
--
-- Each Michelson instruction is represented by exactly one
-- constructor of this data type. Sequence of instructions
-- is represented with use of @Seq@ constructor in following
-- way: @SWAP; DROP ; DUP;@ -> @SWAP `Seq` DROP `Seq` DUP@.
-- Special case where there are no instructions is represented
-- by constructor @Nop@, e.g. @IF_NONE {} { SWAP; DROP; }@ ->
-- @IF_NONE Nop (SWAP `Seq` DROP)@.
--
-- Type parameter @inp@ states for input stack type. That is,
-- type of the stack that is required for operation to execute.
--
-- Type parameter @out@ states for output stack type or type
-- of stack that will be left after instruction's execution.
data Instr (inp :: [T]) (out :: [T]) where
  Seq :: Instr a b -> Instr b c -> Instr a c
