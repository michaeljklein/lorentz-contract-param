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
  , Annotation (..)
  , TypeAnn
  , FieldAnn
  , VarAnn
  , ann
  , noAnn
  , Type (..)
  , Comparable (..)
  , T (..)
  , CT (..)

  -- Parser types
  , CustomParserException (..)
  , Parser
  , ParserException(..)
  , Program (..)
  , getContract
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
  , LetMacro (..)
  , LetValue (..)
  , LetType (..)
  -- * MorleyInstr
  , MorleyInstr(..)
  , Test (..)
  , PrintComment (..)
  , StackRef (..)
  ) where

import Control.Monad.Reader
import Data.Data (Data(..))
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Michelson.Types
  (Annotation(..), CT(..), Comparable(..), Contract(..), Elt(..), FieldAnn, Instr,
  InstrAbstract(..), Op(..), Parameter, Storage, T(..), Type(..), TypeAnn, Value(..), VarAnn, ann,
  noAnn)
import Morley.Default (Default(..))
import Text.Megaparsec
import qualified Text.Show

-------------------------------------
-- Types for the parser
-------------------------------------


type Parser = ReaderT Env (Parsec CustomParserException T.Text)

instance Default a => Default (Parser a) where
  def = pure def

data CustomParserException
  = UnknownTypeException
  | OddNumberBytesException
  | UnexpectedLineBreak
  deriving (Eq, Data, Ord, Show)

instance ShowErrorComponent CustomParserException where
  showErrorComponent UnknownTypeException = "unknown type"
  showErrorComponent OddNumberBytesException = "odd number bytes"
  showErrorComponent UnexpectedLineBreak = "unexpected linebreak"

data ParserException = ParserException (ParseErrorBundle T.Text CustomParserException)

instance Show ParserException where
  show (ParserException bundle) = errorBundlePretty bundle

instance Exception ParserException where
  displayException (ParserException bundle) = errorBundlePretty bundle

-- Parser Environment
type PragmaState = Map Pragma Bool
data Env = Env { pragmas :: PragmaState
               , letMacros :: [LetMacro]
               , letValues :: [LetValue]
               , letTypes  :: [LetType]
               } deriving (Show, Eq)
--
data Program = Program (Contract ParsedOp) Env [Property] deriving (Show, Eq)

getContract :: Program -> Contract ParsedOp
getContract (Program c _ _) = c

data Property = Property deriving (Eq, Show)

data Pragma = XContractMain | XOverloadedPrimitives
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

allPragmas :: [Pragma]
allPragmas = [minBound :: Pragma ..]

mkPragmaState :: [Pragma] -> PragmaState
mkPragmaState ps = Map.fromList $ (,Prelude.False) <$> ps

mkEnv :: [Pragma] -> [LetMacro] -> [LetValue] -> [LetType]-> Env
mkEnv ps = Env (Map.fromList $ (,Prelude.False) <$> ps)

-------------------------------------
-- Types produced by parser
-------------------------------------
type ParsedInstr = InstrAbstract ParsedOp
data ParsedOp
  = PRIM ParsedInstr
  | MAC Macro
  | LETMAC LetMacro
  | MORLEY MorleyInstr
  | SEQ [ParsedOp]
  deriving (Eq, Show, Data)

data MorleyInstr =
    STACK (Stack Type)
  | TEST Test
  | PRINT PrintComment
  deriving (Eq, Show, Data)

-- Stack Type
type Var = T.Text
data TyVar = VarID Var | TyCon Type deriving (Eq, Show, Data)
data Stack a = StkEmpty | StkRest | StkCons a (Stack a) deriving (Eq, Show, Data)
data StackFun = StackFun [Var] (Stack TyVar) (Stack TyVar) deriving (Eq, Show, Data)

-- Let-block
data LetMacro = LetMacro
  { lm_name :: T.Text
  , lm_sig :: StackFun
  , lm_expr :: [ParsedOp]
  } deriving (Eq, Show, Data)

data LetValue = LetValue
  { lv_name :: T.Text
  , lv_sig :: Type
  , lv_val :: (Value ParsedOp)
  } deriving (Eq, Show)

data LetType = LetType
  { lt_name :: T.Text
  , lt_sig :: Type
  } deriving (Eq, Show)

-- Assertion
data Test = Test
  { testName :: T.Text
  , testComment :: PrintComment
  , testInstrs :: [ParsedOp]
  } deriving (Eq, Show, Data)

newtype PrintComment = PrintComment [Either T.Text StackRef] deriving (Eq, Show, Data)
newtype StackRef = StackRef Integer deriving (Eq, Show, Data)

-------------------------------------
-- Types after macroexpander
-------------------------------------
type ExpandedInstr = InstrAbstract ExpandedOp
data ExpandedOp
  = PRIM_EX ExpandedInstr
  | MORLEY_EX MorleyInstr
  | SEQ_EX [ExpandedOp]
  deriving (Eq, Show, Data)

data PairStruct
  = F (VarAnn, FieldAnn)
  | P PairStruct PairStruct
  deriving (Eq, Show, Data)

data CadrStruct
  = A
  | D
  deriving (Eq, Show, Data)

data Macro
  = CMP ParsedInstr VarAnn
  | IFX ParsedInstr [ParsedOp] [ParsedOp]
  | IFCMP ParsedInstr VarAnn [ParsedOp] [ParsedOp]
  | FAIL
  | PAPAIR PairStruct TypeAnn VarAnn
  | UNPAIR PairStruct
  | CADR [CadrStruct] VarAnn FieldAnn
  | SET_CADR [CadrStruct] VarAnn FieldAnn
  | MAP_CADR [CadrStruct] VarAnn FieldAnn [ParsedOp]
  | DIIP Integer [ParsedOp]
  | DUUP Integer VarAnn
  | ASSERT
  | ASSERTX ParsedInstr
  | ASSERT_CMP ParsedInstr
  | ASSERT_NONE
  | ASSERT_SOME
  | ASSERT_LEFT
  | ASSERT_RIGHT
  | IF_SOME [ParsedOp] [ParsedOp]
  deriving (Eq, Show, Data)

