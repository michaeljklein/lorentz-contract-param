{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeFamilies #-}

-- | Module, containing some boilerplate for support of
-- arithmetic operations in Michelson language.

module Michelson.Typed.Arith
  ( ArithOp (..)
  , UnaryArithOp (..)
  , ArithError (..)
  , ArithErrorType (..)
  , CompareOp (..)
  , Add
  , Sub
  , Mul
  , Abs
  , Neg
  , Or
  , And
  , Xor
  , Not
  , Lsl
  , Lsr
  , Compare
  , Eq'
  , Neq
  , Lt
  , Gt
  , Le
  , Ge
  ) where

import Data.Bits (complement, shift, xor, (.&.), (.|.))
import Fmt (Buildable(build))

import qualified Gas.Cost.CostOf as Gas
import Gas.Type (Cost)
import Michelson.Text (MText(..))
import Michelson.Typed.CValue (CValue(..))
import Michelson.Typed.T (CT(..))
import Tezos.Core (addMutez, mulMutez, subMutez, timestampFromSeconds, timestampToSeconds)

-- | Class for binary arithmetic operation.
--
-- Takes binary operation marker as @op@ parameter,
-- types of left operand @n@ and right operand @m@.
class ArithOp aop (n :: CT) (m :: CT) where

  -- | Type family @ArithRes@ denotes the type resulting from
  -- computing operation @op@ from operands of types @n@ and @m@.
  --
  -- For instance, adding integer to natural produces integer,
  -- which is reflected in following instance of type family:
  -- @ArithRes Add CNat CInt = CInt@.
  type ArithRes aop n m :: CT

  -- | Evaluate arithmetic operation on given operands.
  evalOp :: proxy aop -> CValue n -> CValue m -> Either (ArithError (CValue n) (CValue m)) (CValue (ArithRes aop n m))
  costArithOp :: proxy aop -> CValue n -> CValue m -> Cost

-- | Class for comparison operations, special case of 'ArithOp'.
class CompareOp n where
  -- | Evaluate compare operation.
  compareOp :: CValue n -> CValue n -> Integer
  costCompareOp :: CValue n -> CValue n -> Cost

-- | Denotes the error type occured in the arithmetic operation.
data ArithErrorType
  = AddOverflow
  | MulOverflow
  | SubUnderflow
  | LslOverflow
  | LsrUnderflow
  deriving (Show, Eq, Ord)

-- | Represents an arithmetic error of the operation.
data ArithError n m
  = MutezArithError ArithErrorType n m
  | ShiftArithError ArithErrorType n m
  deriving (Show, Eq, Ord)

-- | Marker data type for add operation.
class UnaryArithOp aop (n :: CT) where
  type UnaryArithRes aop n :: CT
  evalUnaryArithOp :: proxy aop -> CValue n -> CValue (UnaryArithRes aop n)
  costUnaryOp :: proxy aop -> CValue n -> Cost

data Add
data Sub
data Mul
data Abs
data Neg

data Or
data And
data Xor
data Not
data Lsl
data Lsr

data Compare
data Eq'
data Neq
data Lt
data Gt
data Le
data Ge

instance ArithOp Add 'CNat 'CInt where
  type ArithRes Add 'CNat 'CInt = 'CInt
  evalOp _ (CvNat i) (CvInt j) = Right $ CvInt (toInteger i + j)
  costArithOp _ (CvNat i) (CvInt j) =
    Gas.add (fromIntegral . toInteger $ i) (fromIntegral j)
instance ArithOp Add 'CInt 'CNat where
  type ArithRes Add 'CInt 'CNat = 'CInt
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i + toInteger j)
  costArithOp _ (CvInt i) (CvNat j) =
    Gas.add (fromIntegral i) (fromIntegral . toInteger $ j)
instance ArithOp Add 'CNat 'CNat where
  type ArithRes Add 'CNat 'CNat = 'CNat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i + j)
  costArithOp _ (CvNat i) (CvNat j) =
    Gas.add (fromIntegral . toInteger $ i) (fromIntegral . toInteger $ j)
instance ArithOp Add 'CInt 'CInt where
  type ArithRes Add 'CInt 'CInt = 'CInt
  evalOp _ (CvInt i) (CvInt j) = Right $ CvInt (i + j)
  costArithOp _ (CvInt i) (CvInt j) =
    Gas.add (fromIntegral i) (fromIntegral j)
instance ArithOp Add 'CTimestamp 'CInt where
  type ArithRes Add 'CTimestamp 'CInt = 'CTimestamp
  evalOp _ (CvTimestamp i) (CvInt j) =
    Right $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds i + j
  costArithOp _ (CvTimestamp i) (CvInt j) = Gas.addTimestamp i (fromIntegral j)
instance ArithOp Add 'CInt 'CTimestamp where
  type ArithRes Add 'CInt 'CTimestamp = 'CTimestamp
  evalOp _ (CvInt i) (CvTimestamp j) =
    Right $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds j + i
  costArithOp _ (CvInt i) (CvTimestamp j) = Gas.addTimestamp j (fromIntegral i)
instance ArithOp Add 'CMutez 'CMutez where
  type ArithRes Add 'CMutez 'CMutez = 'CMutez
  evalOp _ n@(CvMutez i) m@(CvMutez j) = res
    where
      res = maybe (Left $ MutezArithError AddOverflow n m) (Right . CvMutez) $ i `addMutez` j
  costArithOp _ _ _ = Gas.int64Op

instance ArithOp Sub 'CNat 'CInt where
  type ArithRes Sub 'CNat 'CInt = 'CInt
  evalOp _ (CvNat i) (CvInt j) = Right $ CvInt (toInteger i - j)
  costArithOp _ (CvNat i) (CvInt j) =
    Gas.sub (fromIntegral . toInteger $ i) (fromIntegral j)
instance ArithOp Sub 'CInt 'CNat where
  type ArithRes Sub 'CInt 'CNat = 'CInt
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i - toInteger j)
  costArithOp _ (CvInt i) (CvNat j) =
    Gas.sub (fromIntegral i) (fromIntegral . toInteger $ j)
instance ArithOp Sub 'CNat 'CNat where
  type ArithRes Sub 'CNat 'CNat = 'CInt
  evalOp _ (CvNat i) (CvNat j) = Right $ CvInt (toInteger i - toInteger j)
  costArithOp _ (CvNat i) (CvNat j) =
    Gas.sub (fromIntegral . toInteger $ i) (fromIntegral . toInteger $ j)
instance ArithOp Sub 'CInt 'CInt where
  type ArithRes Sub 'CInt 'CInt = 'CInt
  evalOp _ (CvInt i) (CvInt j) = Right $ CvInt (i - j)
  costArithOp _ (CvInt i) (CvInt j) =
    Gas.sub (fromIntegral i) (fromIntegral j)
instance ArithOp Sub 'CTimestamp 'CInt where
  type ArithRes Sub 'CTimestamp 'CInt = 'CTimestamp
  evalOp _ (CvTimestamp i) (CvInt j) =
    Right $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds i - j
  costArithOp _ (CvTimestamp i) (CvInt j) = Gas.subTimestamp i (fromIntegral j)
instance ArithOp Sub 'CTimestamp 'CTimestamp where
  type ArithRes Sub 'CTimestamp 'CTimestamp = 'CInt
  evalOp _ (CvTimestamp i) (CvTimestamp j) =
    Right $ CvInt $ timestampToSeconds i - timestampToSeconds j
  costArithOp _ (CvTimestamp i) (CvTimestamp j) = Gas.diffTimestamps i j
instance ArithOp Sub 'CMutez 'CMutez where
  type ArithRes Sub 'CMutez 'CMutez = 'CMutez
  evalOp _ n@(CvMutez i) m@(CvMutez j) = res
    where
      res = maybe (Left $ MutezArithError SubUnderflow n m) (Right . CvMutez) $ i `subMutez` j
  costArithOp _ _ _ = Gas.int64Op

instance ArithOp Mul 'CNat 'CInt where
  type ArithRes Mul 'CNat 'CInt = 'CInt
  evalOp _ (CvNat i) (CvInt j) = Right $ CvInt (toInteger i * j)
  costArithOp _ (CvNat i) (CvInt j) =
    Gas.mul (fromIntegral . toInteger $ i) (fromIntegral j)
instance ArithOp Mul 'CInt 'CNat where
  type ArithRes Mul 'CInt 'CNat = 'CInt
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i * toInteger j)
  costArithOp _ (CvInt i) (CvNat j) =
    Gas.mul (fromIntegral i) (fromIntegral . toInteger $ j)
instance ArithOp Mul 'CNat 'CNat where
  type ArithRes Mul 'CNat 'CNat = 'CNat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i * j)
  costArithOp _ (CvNat i) (CvNat j) =
    Gas.mul (fromIntegral . toInteger $ i) (fromIntegral . toInteger $ j)
instance ArithOp Mul 'CInt 'CInt where
  type ArithRes Mul 'CInt 'CInt = 'CInt
  evalOp _ (CvInt i) (CvInt j) = Right $ CvInt (i * j)
  costArithOp _ (CvInt i) (CvInt j) =
    Gas.mul (fromIntegral i) (fromIntegral j)
instance ArithOp Mul 'CNat 'CMutez where
  type ArithRes Mul 'CNat 'CMutez = 'CMutez
  evalOp _ n@(CvNat i) m@(CvMutez j) = res
    where
      res = maybe (Left $ MutezArithError MulOverflow n m) (Right . CvMutez) $ j `mulMutez` i
  costArithOp _ _ _ = Gas.int64Op <> Gas.zToInt64
instance ArithOp Mul 'CMutez 'CNat where
  type ArithRes Mul 'CMutez 'CNat = 'CMutez
  evalOp _ n@(CvMutez i) m@(CvNat j) = res
    where
      res = maybe (Left $ MutezArithError MulOverflow n m) (Right . CvMutez) $ i `mulMutez` j
  costArithOp _ _ _ = Gas.int64Op <> Gas.zToInt64

instance UnaryArithOp Abs 'CInt where
  type UnaryArithRes Abs 'CInt = 'CNat
  evalUnaryArithOp _ (CvInt i) = CvNat (fromInteger $ abs i)
  costUnaryOp _ (CvInt i) = Gas.abs i

instance UnaryArithOp Neg 'CInt where
  type UnaryArithRes Neg 'CInt = 'CInt
  evalUnaryArithOp _ (CvInt i) = CvInt (-i)
  costUnaryOp _ (CvInt i) = Gas.neg i
instance UnaryArithOp Neg 'CNat where
  type UnaryArithRes Neg 'CNat = 'CInt
  evalUnaryArithOp _ (CvNat i) = CvInt (- fromIntegral i)
  costUnaryOp _ (CvNat i) = Gas.neg $ fromIntegral i

instance ArithOp Or 'CNat 'CNat where
  type ArithRes Or 'CNat 'CNat = 'CNat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i .|. j)
  costArithOp _ (CvNat i) (CvNat j) =
    Gas.logOr (fromIntegral . toInteger $ i) (fromIntegral . toInteger $ j)
instance ArithOp Or 'CBool 'CBool where
  type ArithRes Or 'CBool 'CBool = 'CBool
  evalOp _ (CvBool i) (CvBool j) = Right $ CvBool (i .|. j)
  costArithOp _ _ _ = Gas.boolBinOp

instance ArithOp And 'CInt 'CNat where
  type ArithRes And 'CInt 'CNat = 'CInt
  evalOp _ (CvInt i) (CvNat j) = Right $ CvInt (i .&. fromIntegral j)
  costArithOp _ (CvInt i) (CvNat j) =
    Gas.logAnd (fromIntegral i) (fromIntegral . toInteger $ j)
instance ArithOp And 'CNat 'CNat where
  type ArithRes And 'CNat 'CNat = 'CNat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i .&. j)
  costArithOp _ (CvNat i) (CvNat j) =
    Gas.logAnd (fromIntegral . toInteger $ i) (fromIntegral . toInteger $ j)
instance ArithOp And 'CBool 'CBool where
  type ArithRes And 'CBool 'CBool = 'CBool
  evalOp _ (CvBool i) (CvBool j) = Right $ CvBool (i .&. j)
  costArithOp _ _ _ = Gas.boolBinOp

instance ArithOp Xor 'CNat 'CNat where
  type ArithRes Xor 'CNat 'CNat = 'CNat
  evalOp _ (CvNat i) (CvNat j) = Right $ CvNat (i `xor` j)
  costArithOp _ (CvNat i) (CvNat j) =
    Gas.logXor (fromIntegral . toInteger $ i) (fromIntegral . toInteger $ j)
instance ArithOp Xor 'CBool 'CBool where
  type ArithRes Xor 'CBool 'CBool = 'CBool
  evalOp _ (CvBool i) (CvBool j) = Right $ CvBool (i `xor` j)
  costArithOp _ _ _ = Gas.boolBinOp

instance ArithOp Lsl 'CNat 'CNat where
  type ArithRes Lsl 'CNat 'CNat = 'CNat
  evalOp _ n@(CvNat i) m@(CvNat j) =
    if j > 256
    then Left $ ShiftArithError LslOverflow n m
    else Right $ CvNat (fromInteger $ shift (toInteger i) (fromIntegral j))
  costArithOp _ (CvNat i) (CvNat j) =
    Gas.shiftLeft (fromIntegral . toInteger $ i) (fromIntegral . toInteger $ j)

instance ArithOp Lsr 'CNat 'CNat where
  type ArithRes Lsr 'CNat 'CNat = 'CNat
  evalOp _ n@(CvNat i) m@(CvNat j) =
    if j > 256
    then Left $ ShiftArithError LsrUnderflow n m
    else Right $ CvNat (fromInteger $ shift (toInteger i) (-(fromIntegral j)))
  costArithOp _ (CvNat i) (CvNat j) =
    Gas.shiftRight (fromIntegral . toInteger $ i) (fromIntegral . toInteger $ j)

instance UnaryArithOp Not 'CInt where
  type UnaryArithRes Not 'CInt = 'CInt
  evalUnaryArithOp _ (CvInt i) = CvInt (complement i)
  costUnaryOp _ (CvInt i) = Gas.logNot (fromIntegral i)
instance UnaryArithOp Not 'CNat where
  type UnaryArithRes Not 'CNat = 'CInt
  evalUnaryArithOp _ (CvNat i) = CvInt (complement $ toInteger i)
  costUnaryOp _ (CvNat i) = Gas.logNot (fromIntegral . toInteger $ i)
instance UnaryArithOp Not 'CBool where
  type UnaryArithRes Not 'CBool = 'CBool
  evalUnaryArithOp _ (CvBool i) = CvBool (not i)
  costUnaryOp _ _ = Gas.boolUnOp

instance (n ~ m, CompareOp n) => ArithOp Compare n m where
  type ArithRes Compare n m = 'CInt
  evalOp _ = Right . CvInt ... compareOp
  costArithOp _ n m = costCompareOp n m
instance CompareOp 'CBool where
  compareOp (CvBool i) (CvBool j) =
    toInteger $ fromEnum (compare i j) - 1
  costCompareOp _ _ = Gas.compareBool
instance CompareOp 'CAddress where
  compareOp (CvAddress i) (CvAddress j) =
    toInteger $ fromEnum (compare i j) - 1
  costCompareOp _ _ = Gas.compareAddress
instance CompareOp 'CNat where
  compareOp (CvNat i) (CvNat j) =
    toInteger $ fromEnum (compare i j) - 1
  costCompareOp (CvNat i) (CvNat j) = Gas.compareNat i j
instance CompareOp 'CInt where
  compareOp (CvInt i) (CvInt j) =
    toInteger $ fromEnum (compare i j) - 1
  costCompareOp (CvInt i) (CvInt j) = Gas.compareInt i j
instance CompareOp 'CString where
  compareOp (CvString i) (CvString j) =
    toInteger $ fromEnum (compare i j) - 1
  costCompareOp (CvString i) (CvString j) =
    Gas.compareString (unMText i) (unMText j)
instance CompareOp 'CBytes where
  compareOp (CvBytes i) (CvBytes j) =
    toInteger $ fromEnum (compare i j) - 1
  costCompareOp (CvBytes i) (CvBytes j) = Gas.compareBytes i j
instance CompareOp 'CTimestamp where
  compareOp (CvTimestamp i) (CvTimestamp j) =
    toInteger $ fromEnum (compare i j) - 1
  costCompareOp (CvTimestamp i) (CvTimestamp j) = Gas.compareTimestamp i j
instance CompareOp 'CMutez where
  compareOp (CvMutez i) (CvMutez j) =
    toInteger $ fromEnum (compare i j) - 1
  costCompareOp _ _ = Gas.compareTez
instance CompareOp 'CKeyHash where
  compareOp (CvKeyHash i) (CvKeyHash j) =
    toInteger $ fromEnum (compare i j) - 1
  costCompareOp _ _ = Gas.compareKeyHash

instance UnaryArithOp Eq' 'CInt where
  type UnaryArithRes Eq' 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i == 0)
  costUnaryOp _ _ = Gas.compareRes

instance UnaryArithOp Neq 'CInt where
  type UnaryArithRes Neq 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i /= 0)
  costUnaryOp _ _ = Gas.compareRes

instance UnaryArithOp Lt 'CInt where
  type UnaryArithRes Lt 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i < 0)
  costUnaryOp _ _ = Gas.compareRes

instance UnaryArithOp Gt 'CInt where
  type UnaryArithRes Gt 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i > 0)
  costUnaryOp _ _ = Gas.compareRes

instance UnaryArithOp Le 'CInt where
  type UnaryArithRes Le 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i <= 0)
  costUnaryOp _ _ = Gas.compareRes

instance UnaryArithOp Ge 'CInt where
  type UnaryArithRes Ge 'CInt = 'CBool
  evalUnaryArithOp _ (CvInt i) = CvBool (i >= 0)
  costUnaryOp _ _ = Gas.compareRes


instance Buildable ArithErrorType where
  build AddOverflow = "add overflow"
  build MulOverflow = "mul overflow"
  build SubUnderflow = "sub overflow"
  build LslOverflow = "lsl overflow"
  build LsrUnderflow = "lsr underflow"

instance (Show n, Show m) => Buildable (ArithError n m) where
  build (MutezArithError errType n m) = "Mutez "
    <> build errType <> " with " <> show n <> ", " <> show m
  build (ShiftArithError errType n m) =
    build errType <> " with " <> show n <> ", " <> show m
