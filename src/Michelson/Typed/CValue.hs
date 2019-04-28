module Michelson.Typed.CValue
  ( CValue (..)
  , ToCVal (..)
  ) where

import Universum

import Michelson.Typed.T (CT(..), ToCT)

data CValue t where
  CvInt       :: Integer -> CValue 'CInt

deriving instance Show (CValue t)
deriving instance Eq (CValue t)

class ToCVal a where
  toCVal :: a -> CValue (ToCT a)

instance ToCVal Integer where
  toCVal = CvInt
