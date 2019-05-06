{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Foundation of Lorentz development.
module Lorentz.Base
  ( (:->) (..)
  , type (&)
  , (#)

  , compileLorentz
  , compileLorentzContract
  , printLorentzContract
  , runLambda

  , Contract
  , Lambda
  , Entry

  , Coercible_
  , coerce_
  ) where

import qualified Data.Kind as Kind
import Data.Singletons (SingI(..))
import qualified Data.Text.Lazy as TL
import Data.Vinyl (Rec(..))

import Lorentz.Value
import Michelson.Interpret (EvalOp, runInstr)
import Michelson.Printer (printTypedContract)
import Michelson.Typed (Instr(..), T(..), ToT, ToTs, Value'(..))

-- | Alias for instruction which hides inner types representation via 'T'.
newtype (inp :: [Kind.Type]) :-> (out :: [Kind.Type]) =
  I { unI :: Instr (ToTs inp) (ToTs out) }
infixr 1 :->

-- | For use outside of Lorentz.
compileLorentz :: (inp :-> out) -> Instr (ToTs inp) (ToTs out)
compileLorentz = unI

-- | Version of 'compileLorentz' specialized to instruction corresponding to
-- contract code.
compileLorentzContract
  :: forall cp st inp out.
     (inp ~ '[(cp, st)], out ~ '[([Operation], st)])
  => (inp :-> out) -> Instr (ToTs inp) (ToTs out)
compileLorentzContract = compileLorentz

printLorentzContract
    :: forall cp st inp out.
     (inp ~ '[(cp, st)], out ~ '[([Operation], st)] , SingI (ToT cp), SingI (ToT st))
  => (inp :-> out) -> TL.Text
printLorentzContract = printTypedContract . compileLorentzContract

-- TODO: Make this better, right now `out` can be any stack
runLambda :: (IsoValue a, ToTs inp ~ '[ToT a])
          => (inp :-> out) -> a -> EvalOp (Rec Value (ToTs out))
runLambda l i = runInstr (compileLorentz l) ((toVal i) :& RNil)

type (&) (a :: Kind.Type) (b :: [Kind.Type]) = a ': b
infixr 2 &

(#) :: (a :-> b) -> (b :-> c) -> a :-> c
I l # I r = I (l `Seq` r)

type Contract cp st = '[(cp, st)] :-> '[([Operation], st)]

type Lambda i o = '[i] :-> '[o]
type Entry st a = '[a, st] :-> '[ ([Operation], st) ]

instance IsoValue (Lambda inp out) where
  type ToT (Lambda inp out) = 'TLambda (ToT inp) (ToT out)
  toVal = VLam . unI
  fromVal (VLam l) = I l

-- | Whether two types have the same Michelson representation.
type Coercible_ a b = ToT a ~ ToT b

-- | Convert between values of types that have the same representation.
coerce_ :: Coercible_ a b => a & s :-> b & s
coerce_ = I Nop
