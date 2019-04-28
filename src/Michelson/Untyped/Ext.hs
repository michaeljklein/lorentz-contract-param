{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

module Michelson.Untyped.Ext
  ( ExtInstrAbstract (..)
  , StackRef (..)
  , PrintComment (..)
  , TestAssert (..)
  , Var (..)
  , TyVar (..)
  , StackTypePattern (..)
  , StackFn (..)
  , varSet
  , stackTypePatternToList
  ) where

import Data.Data (Data(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Michelson.Untyped.Type

-- | Implementation-specific instructions embedded in a @NOP@ primitive, which
-- mark a specific point during a contract's typechecking or execution.
--
-- These instructions are not allowed to modify the contract's stack, but may
-- impose additional constraints that can cause a contract to report errors in
-- type-checking or testing.
--
-- Additionaly, some implementation-specific language features such as
-- type-checking of @LetMacro@s are implemented using this mechanism
-- (specifically @FN@ and @FN_END@).
data ExtInstrAbstract op =
    STACKTYPE StackTypePattern -- ^ Matches current stack against a type-pattern
  | FN T.Text StackFn [op]     -- ^ A typed stack function (push and pop a @TcExtFrame@)
  | UTEST_ASSERT (TestAssert op)   -- ^ Copy the current stack and run an inline assertion on it
  | UPRINT PrintComment         -- ^ Print a comment with optional embedded @StackRef@s
  deriving (Eq, Show, Data, Generic, Functor)

-- | A reference into the stack.
newtype StackRef = StackRef Natural
  deriving (Eq, Show, Data, Generic)

newtype Var = Var T.Text deriving (Eq, Show, Ord, Data, Generic)

-- | A type-variable or a type-constant
data TyVar =
    VarID Var
  | TyCon Type
  deriving (Eq, Show, Data, Generic)

-- | A stack pattern-match
data StackTypePattern
 = StkEmpty
 | StkRest
 | StkCons TyVar StackTypePattern
  deriving (Eq, Show, Data, Generic)

-- | Convert 'StackTypePattern' to a list of types. Also returns
-- 'Bool' which is 'True' if the pattern is a fixed list of types and
-- 'False' if it's a pattern match on the head of the stack.
stackTypePatternToList :: StackTypePattern -> ([TyVar], Bool)
stackTypePatternToList StkEmpty = ([], True)
stackTypePatternToList StkRest = ([], False)
stackTypePatternToList (StkCons t pat) =
  first (t :) $ stackTypePatternToList pat

-- | A stack function that expresses the type signature of a @LetMacro@
data StackFn = StackFn
  { quantifiedVars :: Maybe (Set Var)
  , inPattern :: StackTypePattern
  , outPattern :: StackTypePattern
  } deriving (Eq, Show, Data, Generic)

-- | Get the set of variables in a stack pattern
varSet :: StackTypePattern -> Set Var
varSet StkEmpty = Set.empty
varSet StkRest = Set.empty
varSet (StkCons (VarID v) stk) = v `Set.insert` (varSet stk)
varSet (StkCons _ stk) = varSet stk

newtype PrintComment = PrintComment
  { unUPrintComment :: [Either T.Text StackRef]
  } deriving (Eq, Show, Data, Generic)

-- An inline test assertion
data TestAssert op = TestAssert
  { tassName :: T.Text
  , tassComment :: PrintComment
  , tassInstrs :: [op]
  } deriving (Eq, Show, Functor, Data, Generic)
