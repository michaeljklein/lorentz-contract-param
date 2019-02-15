{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Morley.Types
  (
   -- * Rexported from Michelson.Types
    Parameter
  , Storage
  , Contract (..)
  , Value (..)
  , Elt (..)
  , InstrAbstract (..)
  , Instr
  , Op (..)
  , TypeNote
  , FieldNote
  , VarNote
  , Type (..)
  , Comparable (..)
  , T (..)
  , CT (..)

  -- Parser types
  , Parser
  , ParserException(..)
  , Program (..)
  , Pragma (..)
  , allPragmas
  , mkEnv
  , Env (..)

  -- * Typechecker types
  , ExpandedInstr
  , ExpandedOp (..)

  -- * Michelson Instructions and Instruction Macros
  , ParsedOp (..)
  , PairStruct (..)
  , CadrStruct (..)
  , Macro (..)
  , ParsedInstr

  -- * Stack
  , Stack(..)
  , StackFun(..)
  , Var
  , TyVar(..)
  , CustomMacro (..)
  ) where

import Control.Monad.Reader
import Data.Data (Data(..))
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Michelson.Types
  (CT(..), Comparable(..), Contract(..), Elt(..), FieldNote, Instr, InstrAbstract(..), Op(..),
  Parameter, Storage, T(..), Type(..), TypeNote, Value(..), VarNote)
import Morley.Default (Default(..))
import Text.Megaparsec
-------------------------------------
-- Types for the parser
-------------------------------------

type PragmaState = Map Pragma Bool
type CMacroState = [CustomMacro]
data Env = Env { pragmas :: PragmaState, cmacros :: CMacroState }
  deriving (Show, Eq)

type Parser = ReaderT Env (Parsec Void T.Text)

instance Default a => Default (Parser a) where
  def = pure def

data ParserException = ParserException (ParseErrorBundle T.Text Void)
  deriving (Show)

instance Exception ParserException where
  displayException (ParserException bundle) = errorBundlePretty bundle

data Program = Program (Contract ParsedOp) Env
  deriving (Show, Eq)

data Pragma = XContractMain
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

allPragmas :: [Pragma]
allPragmas = [minBound :: Pragma ..]

mkPragmaState :: [Pragma] -> PragmaState
mkPragmaState ps = Map.fromList $ (,Prelude.False) <$> ps

mkEnv :: [Pragma] -> [CustomMacro] -> Env
mkEnv ps cs = Env (Map.fromList $ (,Prelude.False) <$> ps) cs

-------------------------------------
-- Types produced by parser
-------------------------------------
type ParsedInstr = InstrAbstract ParsedOp
data ParsedOp =
    PRIM ParsedInstr
  | MAC Macro
  | CMAC CustomMacro
  | SEQ [ParsedOp]
  deriving (Eq, Show)

-------------------------------------
-- Types after macroexpander
-------------------------------------
type ExpandedInstr = InstrAbstract ExpandedOp
data ExpandedOp =
    PRIM_EX ExpandedInstr
  | SEQ_EX [ExpandedOp]
  deriving (Eq, Show, Data)

data PairStruct = F (VarNote, FieldNote) | P PairStruct PairStruct
  deriving (Eq, Show)
data CadrStruct = A | D deriving (Eq, Show)

data Macro =
    CMP ParsedInstr VarNote
  | IFX ParsedInstr [ParsedOp] [ParsedOp]
  | IFCMP ParsedInstr VarNote [ParsedOp] [ParsedOp]
  | FAIL
  | PAPAIR PairStruct TypeNote VarNote
  | UNPAIR PairStruct
  | CADR [CadrStruct] VarNote FieldNote
  | SET_CADR [CadrStruct] VarNote FieldNote
  | MAP_CADR [CadrStruct] VarNote FieldNote [ParsedOp]
  | DIIP Integer [ParsedOp]
  | DUUP Integer VarNote
  | ASSERT
  | ASSERTX ParsedInstr
  | ASSERT_CMP ParsedInstr
  | ASSERT_NONE
  | ASSERT_SOME
  | ASSERT_LEFT
  | ASSERT_RIGHT
  | IF_SOME [ParsedOp] [ParsedOp]
  deriving (Eq, Show)

-- Stack Type
type Var = T.Text
data TyVar = VarID Var | TyCon Type deriving (Eq, Show)

data Stack a = StkEmpty | StkRest | StkCons a (Stack a) deriving (Eq, Show)

data StackFun = StackFun [Var] (Stack TyVar) (Stack TyVar) deriving (Eq, Show)

data CustomMacro = CustomMacro
  { cm_name :: T.Text
  , cm_sig :: StackFun
  , cm_expr :: [ParsedOp]
  } deriving (Eq, Show)

