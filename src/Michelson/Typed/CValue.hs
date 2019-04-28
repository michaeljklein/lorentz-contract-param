-- | Module, containing CValue data type
-- which represents Michelson comparable values.

module Michelson.Typed.CValue
  ( CValue (..)
  , ToCVal
  , toCVal
  ) where

import Michelson.Typed.T (CT(..), ToCT)

data CValue t where
  CvInt       :: Integer -> CValue 'CInt
  CvNat       :: Natural -> CValue 'CNat
  CvString    :: Text -> CValue 'CString
  CvBytes     :: ByteString -> CValue 'CBytes
  CvBool      :: Bool -> CValue 'CBool

deriving instance Show (CValue t)
deriving instance Eq (CValue t)
deriving instance Ord (CValue t)

-- | Converts a single Haskell value into @CVal@ representation.
class ToCVal a where
  toCVal :: a -> CValue (ToCT a)

instance ToCVal Integer where
  toCVal = CvInt
