{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeFamilies #-}

-- | Module, containing some boilerplate for support of
-- arithmetic operations in Michelson language.

module Michelson.Typed.Arith
  ( ArithOp (..)
  , UnaryArithOp (..)
  , ArithError (..)
  , ArithErrorType (..)
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

import Michelson.Typed.CValue (CVal(..))
import Michelson.Typed.Value (Val(VC, VAnn))
import Michelson.Typed.T (T(..), CT(..))
import Tezos.Core (addMutez, mulMutez, subMutez, timestampFromSeconds, timestampToSeconds)

-- | Class for binary arithmetic operation.
--
-- Takes binary operation marker as @op@ parameter,
-- types of left operand @n@ and right operand @m@.
class ArithOp aop (n :: T) (m :: T) where

  -- | Type family @ArithRes@ denotes the type resulting from
  -- computing operation @op@ from operands of types @n@ and @m@.
  --
  -- For instance, adding integer to natural produces integer,
  -- which is reflected in following instance of type family:
  -- @ArithRes Add T_nat T_int = T_int@.
  type ArithRes aop n m :: T

  -- | Evaluate arithmetic operation on given operands.
  evalOp
    :: proxy aop -> Val instr n -> Val instr m
    -> Either (ArithError (Val instr n) (Val instr m)) (Val instr (ArithRes aop n m))

-- | Denotes the error type occured in the arithmetic operation.
data ArithErrorType
  = AddOverflow
  | MulOverflow
  | SubUnderflow
  deriving (Show, Eq, Ord)

-- | Represents an arithmetic error of the operation.
data ArithError n m
  = MutezArithError ArithErrorType n m
  deriving (Show, Eq, Ord)

-- | Marker data type for add operation.
class UnaryArithOp aop (n :: T) where
  type UnaryArithRes aop n :: T
  evalUnaryArithOp :: proxy aop -> Val instr n -> Val instr (UnaryArithRes aop n)

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

instance (ArithOp aop a b)
  => ArithOp aop ('T_custom ann a) ('T_custom ann b) where
  type ArithRes aop ('T_custom ann a) ('T_custom ann b) =
    'T_custom ann (ArithRes aop a b)
  evalOp aop (VAnn i) (VAnn j) =
    case evalOp aop i j of
      Left (MutezArithError eType _ _) -> Left $ MutezArithError eType (VAnn i) (VAnn j)
      Right v -> Right $ VAnn v

instance (ArithOp aop ('T_c a) ('T_c b))
  => ArithOp aop ('T_custom ann ('T_c a)) ('T_c b) where
  type ArithRes aop ('T_custom ann ('T_c a)) ('T_c b) =
    'T_custom ann (ArithRes aop ('T_c a) ('T_c b))
  evalOp aop (VAnn i) j =
    case evalOp aop i j of
      Left (MutezArithError eType _ _) -> Left $ MutezArithError eType (VAnn i) j
      Right v -> Right $ VAnn v

instance (ArithOp aop ('T_c a) ('T_c b))
  => ArithOp aop ('T_c a) ('T_custom ann ('T_c b)) where
  type ArithRes aop ('T_c a) ('T_custom ann ('T_c b)) =
    'T_custom ann (ArithRes aop ('T_c a) ('T_c b))
  evalOp aop i (VAnn j) =
    case evalOp aop i j of
      Left (MutezArithError eType _ _) -> Left $ MutezArithError eType i (VAnn j)
      Right v -> Right $ VAnn v

instance (UnaryArithOp aop a)
  => UnaryArithOp aop ('T_custom ann a) where
  type UnaryArithRes aop ('T_custom ann a) = 'T_custom ann (UnaryArithRes aop a)
  evalUnaryArithOp aop (VAnn i) = VAnn $ evalUnaryArithOp aop i

instance ArithOp Add ('T_c 'T_nat) ('T_c 'T_int) where
  type ArithRes Add ('T_c 'T_nat) ('T_c 'T_int) = 'T_c 'T_int
  evalOp _ (VC (CvNat i)) (VC (CvInt j)) = Right $ VC $ CvInt (toInteger i + j)
instance ArithOp Add ('T_c 'T_int) ('T_c 'T_nat) where
  type ArithRes Add ('T_c 'T_int) ('T_c 'T_nat) = 'T_c 'T_int
  evalOp _ (VC (CvInt i)) (VC (CvNat j)) = Right $ VC $ CvInt (i + toInteger j)
instance ArithOp Add ('T_c 'T_nat) ('T_c 'T_nat) where
  type ArithRes Add ('T_c 'T_nat) ('T_c 'T_nat) = 'T_c 'T_nat
  evalOp _ (VC (CvNat i)) (VC (CvNat j)) = Right $ VC $ CvNat (i + j)
instance ArithOp Add ('T_c 'T_int) ('T_c 'T_int) where
  type ArithRes Add ('T_c 'T_int) ('T_c 'T_int) = 'T_c 'T_int
  evalOp _ (VC (CvInt i)) (VC (CvInt j)) = Right $ VC $ CvInt (i + j)
instance ArithOp Add ('T_c 'T_timestamp) ('T_c 'T_int) where
  type ArithRes Add ('T_c 'T_timestamp) ('T_c 'T_int) = ('T_c 'T_timestamp)
  evalOp _ (VC (CvTimestamp i)) (VC (CvInt j)) =
    Right $ VC $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds i + j
instance ArithOp Add ('T_c 'T_int) ('T_c 'T_timestamp) where
  type ArithRes Add ('T_c 'T_int) ('T_c 'T_timestamp) = ('T_c 'T_timestamp)
  evalOp _ (VC (CvInt i)) (VC (CvTimestamp j)) =
    Right $ VC $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds j + i
instance ArithOp Add ('T_c 'T_mutez) ('T_c 'T_mutez) where
  type ArithRes Add ('T_c 'T_mutez) ('T_c 'T_mutez) = ('T_c 'T_mutez)
  evalOp _ n@(VC (CvMutez i)) m@(VC (CvMutez j)) = res
    where
      res = maybe (Left $ MutezArithError AddOverflow n m) (Right . VC . CvMutez) $ i `addMutez` j

instance ArithOp Sub ('T_c 'T_nat) ('T_c 'T_int) where
  type ArithRes Sub ('T_c 'T_nat) ('T_c 'T_int) = ('T_c 'T_int)
  evalOp _ (VC (CvNat i)) (VC (CvInt j)) = Right $ VC $ CvInt (toInteger i - j)
instance ArithOp Sub ('T_c 'T_int) ('T_c 'T_nat) where
  type ArithRes Sub ('T_c 'T_int) ('T_c 'T_nat) = ('T_c 'T_int)
  evalOp _ (VC (CvInt i)) (VC (CvNat j)) = Right $ VC $ CvInt (i - toInteger j)
instance ArithOp Sub ('T_c 'T_nat) ('T_c 'T_nat) where
  type ArithRes Sub ('T_c 'T_nat) ('T_c 'T_nat) = ('T_c 'T_int)
  evalOp _ (VC (CvNat i)) (VC (CvNat j)) = Right $ VC $ CvInt (toInteger i - toInteger j)
instance ArithOp Sub ('T_c 'T_int) ('T_c 'T_int) where
  type ArithRes Sub ('T_c 'T_int) ('T_c 'T_int) = ('T_c 'T_int)
  evalOp _ (VC (CvInt i)) (VC (CvInt j)) = Right $ VC $ CvInt (i - j)
instance ArithOp Sub ('T_c 'T_timestamp) ('T_c 'T_int) where
  type ArithRes Sub ('T_c 'T_timestamp) ('T_c 'T_int) = ('T_c 'T_timestamp)
  evalOp _ (VC (CvTimestamp i)) (VC (CvInt j)) =
    Right $ VC $ CvTimestamp $ timestampFromSeconds $ timestampToSeconds i - j
instance ArithOp Sub ('T_c 'T_timestamp) ('T_c 'T_timestamp) where
  type ArithRes Sub ('T_c 'T_timestamp) ('T_c 'T_timestamp) = ('T_c 'T_int)
  evalOp _ (VC (CvTimestamp i)) (VC (CvTimestamp j)) =
    Right $ VC $ CvInt $ timestampToSeconds i - timestampToSeconds j
instance ArithOp Sub ('T_c 'T_mutez) ('T_c 'T_mutez) where
  type ArithRes Sub ('T_c 'T_mutez) ('T_c 'T_mutez) = ('T_c 'T_mutez)
  evalOp _ n@(VC (CvMutez i)) m@(VC (CvMutez j)) = res
    where
      res = maybe (Left $ MutezArithError SubUnderflow n m) (Right . VC . CvMutez) $ i `subMutez` j

instance ArithOp Mul ('T_c 'T_nat) ('T_c 'T_int) where
  type ArithRes Mul ('T_c 'T_nat) ('T_c 'T_int) = ('T_c 'T_int)
  evalOp _ (VC (CvNat i)) (VC (CvInt j)) = Right $ VC $ CvInt (toInteger i * j)
instance ArithOp Mul ('T_c 'T_int) ('T_c 'T_nat) where
  type ArithRes Mul ('T_c 'T_int) ('T_c 'T_nat) = ('T_c 'T_int)
  evalOp _ (VC (CvInt i)) (VC (CvNat j)) = Right $ VC $ CvInt (i * toInteger j)
instance ArithOp Mul ('T_c 'T_nat) ('T_c 'T_nat) where
  type ArithRes Mul ('T_c 'T_nat) ('T_c 'T_nat) = ('T_c 'T_nat)
  evalOp _ (VC (CvNat i)) (VC (CvNat j)) = Right $ VC $ CvNat (i * j)
instance ArithOp Mul ('T_c 'T_int) ('T_c 'T_int) where
  type ArithRes Mul ('T_c 'T_int) ('T_c 'T_int) = ('T_c 'T_int)
  evalOp _ (VC (CvInt i)) (VC (CvInt j)) = Right $ VC $ CvInt (i * j)
instance ArithOp Mul ('T_c 'T_nat) ('T_c 'T_mutez) where
  type ArithRes Mul ('T_c 'T_nat) ('T_c 'T_mutez) = ('T_c 'T_mutez)
  evalOp _ n@(VC (CvNat i)) m@(VC (CvMutez j)) = res
    where
      res = maybe (Left $ MutezArithError MulOverflow n m) (Right . VC . CvMutez) $ j `mulMutez` i
instance ArithOp Mul ('T_c 'T_mutez) ('T_c 'T_nat) where
  type ArithRes Mul ('T_c 'T_mutez) ('T_c 'T_nat) = ('T_c 'T_mutez)
  evalOp _ n@(VC (CvMutez i)) m@(VC (CvNat j)) = res
    where
      res = maybe (Left $ MutezArithError MulOverflow n m) (Right . VC . CvMutez) $ i `mulMutez` j

instance UnaryArithOp Abs ('T_c 'T_int) where
  type UnaryArithRes Abs ('T_c 'T_int) = ('T_c 'T_nat)
  evalUnaryArithOp _ (VC (CvInt i)) = VC $ CvNat (fromInteger $ abs i)

instance UnaryArithOp Neg ('T_c 'T_int) where
  type UnaryArithRes Neg ('T_c 'T_int) = ('T_c 'T_int)
  evalUnaryArithOp _ (VC (CvInt i)) = VC $ CvInt (-i)

instance ArithOp Or ('T_c 'T_nat) ('T_c 'T_nat) where
  type ArithRes Or ('T_c 'T_nat) ('T_c 'T_nat) = ('T_c 'T_nat)
  evalOp _ (VC (CvNat i)) (VC (CvNat j)) = Right $ VC $ CvNat (i .|. j)
instance ArithOp Or ('T_c 'T_bool) ('T_c 'T_bool) where
  type ArithRes Or ('T_c 'T_bool) ('T_c 'T_bool) = ('T_c 'T_bool)
  evalOp _ (VC (CvBool i)) (VC (CvBool j)) = Right $ VC $ CvBool (i .|. j)

instance ArithOp And ('T_c 'T_int) ('T_c 'T_nat) where
  type ArithRes And ('T_c 'T_int) ('T_c 'T_nat) = ('T_c 'T_int)
  evalOp _ (VC (CvInt i)) (VC (CvNat j)) = Right $ VC $ CvInt (i .&. fromIntegral j)
instance ArithOp And ('T_c 'T_nat) ('T_c 'T_nat) where
  type ArithRes And ('T_c 'T_nat) ('T_c 'T_nat) = ('T_c 'T_nat)
  evalOp _ (VC (CvNat i)) (VC (CvNat j)) = Right $ VC $ CvNat (i .&. j)
instance ArithOp And ('T_c 'T_bool) ('T_c 'T_bool) where
  type ArithRes And ('T_c 'T_bool) ('T_c 'T_bool) = ('T_c 'T_bool)
  evalOp _ (VC (CvBool i)) (VC (CvBool j)) = Right $ VC $ CvBool (i .&. j)

instance ArithOp Xor ('T_c 'T_nat) ('T_c 'T_nat) where
  type ArithRes Xor ('T_c 'T_nat) ('T_c 'T_nat) = ('T_c 'T_nat)
  evalOp _ (VC (CvNat i)) (VC (CvNat j)) = Right $ VC $ CvNat (i `xor` j)
instance ArithOp Xor ('T_c 'T_bool) ('T_c 'T_bool) where
  type ArithRes Xor ('T_c 'T_bool) ('T_c 'T_bool) = ('T_c 'T_bool)
  evalOp _ (VC (CvBool i)) (VC (CvBool j)) = Right $ VC $ CvBool (i `xor` j)

-- Todo add condition when shift >= 256
instance ArithOp Lsl ('T_c 'T_nat) ('T_c 'T_nat) where
  type ArithRes Lsl ('T_c 'T_nat) ('T_c 'T_nat) = ('T_c 'T_nat)
  evalOp _ (VC (CvNat i)) (VC (CvNat j)) =
    Right $ VC $ CvNat (fromInteger $ shift (toInteger i) (fromIntegral j))

instance ArithOp Lsr ('T_c 'T_nat) ('T_c 'T_nat) where
  type ArithRes Lsr ('T_c 'T_nat) ('T_c 'T_nat) = ('T_c 'T_nat)
  evalOp _ (VC (CvNat i)) (VC (CvNat j)) =
    Right $ VC $ CvNat (fromInteger $ shift (toInteger i) (-(fromIntegral j)))

instance UnaryArithOp Not ('T_c 'T_int) where
  type UnaryArithRes Not ('T_c 'T_int) = ('T_c 'T_int)
  evalUnaryArithOp _ (VC (CvInt i)) = VC $ CvInt (complement i)
instance UnaryArithOp Not ('T_c 'T_nat) where
  type UnaryArithRes Not ('T_c 'T_nat) = ('T_c 'T_int)
  evalUnaryArithOp _ (VC (CvNat i)) = VC $ CvInt (complement $ toInteger i)
instance UnaryArithOp Not ('T_c 'T_bool) where
  type UnaryArithRes Not ('T_c 'T_bool) = ('T_c 'T_bool)
  evalUnaryArithOp _ (VC (CvBool i)) = VC $ CvBool (not i)

instance ArithOp Compare ('T_c 'T_bool) ('T_c 'T_bool) where
  type ArithRes Compare ('T_c 'T_bool) ('T_c 'T_bool) = ('T_c 'T_int)
  evalOp _ (VC (CvBool i)) (VC (CvBool j)) =
    Right $ VC $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare ('T_c 'T_address) ('T_c 'T_address) where
  type ArithRes Compare ('T_c 'T_address) ('T_c 'T_address) = ('T_c 'T_int)
  evalOp _ (VC (CvAddress i)) (VC (CvAddress j)) =
    Right $ VC $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare ('T_c 'T_nat) ('T_c 'T_nat) where
  type ArithRes Compare ('T_c 'T_nat) ('T_c 'T_nat) = ('T_c 'T_int)
  evalOp _ (VC (CvNat i)) (VC (CvNat j)) =
    Right $ VC $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare ('T_c 'T_int) ('T_c 'T_int) where
  type ArithRes Compare ('T_c 'T_int) ('T_c 'T_int) = ('T_c 'T_int)
  evalOp _ (VC (CvInt i)) (VC (CvInt j)) =
    Right $ VC $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare ('T_c 'T_string) ('T_c 'T_string) where
  type ArithRes Compare ('T_c 'T_string) ('T_c 'T_string) = ('T_c 'T_int)
  evalOp _ (VC (CvString i)) (VC (CvString j)) =
    Right $ VC $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare ('T_c 'T_bytes) ('T_c 'T_bytes) where
  type ArithRes Compare ('T_c 'T_bytes) ('T_c 'T_bytes) = ('T_c 'T_int)
  evalOp _ (VC (CvBytes i)) (VC (CvBytes j)) =
    Right $ VC $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare ('T_c 'T_timestamp) ('T_c 'T_timestamp) where
  type ArithRes Compare ('T_c 'T_timestamp) ('T_c 'T_timestamp) = ('T_c 'T_int)
  evalOp _ (VC (CvTimestamp i)) (VC (CvTimestamp j)) =
    Right $ VC $ CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare ('T_c 'T_mutez) ('T_c 'T_mutez) where
  type ArithRes Compare ('T_c 'T_mutez) ('T_c 'T_mutez) = ('T_c 'T_int)
  evalOp _ (VC (CvMutez i)) (VC (CvMutez j)) = Right $ VC $
    CvInt $ toInteger $ fromEnum (compare i j) - 1
instance ArithOp Compare ('T_c 'T_key_hash) ('T_c 'T_key_hash) where
  type ArithRes Compare ('T_c 'T_key_hash) ('T_c 'T_key_hash) = ('T_c 'T_int)
  evalOp _ (VC (CvKeyHash i)) (VC (CvKeyHash j)) =
    Right $ VC $ CvInt $ toInteger $ fromEnum (compare i j) - 1

instance UnaryArithOp Eq' ('T_c 'T_int) where
  type UnaryArithRes Eq' ('T_c 'T_int) = ('T_c 'T_bool)
  evalUnaryArithOp _ (VC (CvInt i)) = VC $ CvBool (i == 0)

instance UnaryArithOp Neq ('T_c 'T_int) where
  type UnaryArithRes Neq ('T_c 'T_int) = ('T_c 'T_bool)
  evalUnaryArithOp _ (VC (CvInt i)) = VC $ CvBool (i /= 0)

instance UnaryArithOp Lt ('T_c 'T_int) where
  type UnaryArithRes Lt ('T_c 'T_int) = ('T_c 'T_bool)
  evalUnaryArithOp _ (VC (CvInt i)) = VC $ CvBool (i < 0)

instance UnaryArithOp Gt ('T_c 'T_int) where
  type UnaryArithRes Gt ('T_c 'T_int) = ('T_c 'T_bool)
  evalUnaryArithOp _ (VC (CvInt i)) = VC $ CvBool (i > 0)

instance UnaryArithOp Le ('T_c 'T_int) where
  type UnaryArithRes Le ('T_c 'T_int) = ('T_c 'T_bool)
  evalUnaryArithOp _ (VC (CvInt i)) = VC $ CvBool (i <= 0)

instance UnaryArithOp Ge ('T_c 'T_int) where
  type UnaryArithRes Ge ('T_c 'T_int) = ('T_c 'T_bool)
  evalUnaryArithOp _ (VC (CvInt i)) = VC $ CvBool (i >= 0)


instance Buildable ArithErrorType where
  build AddOverflow = "add overflow"
  build MulOverflow = "mul overflow"
  build SubUnderflow = "sub overflow"

instance (Show n, Show m) => Buildable (ArithError n m) where
  build (MutezArithError errType n m) = "Mutez "
    <> build errType <> " with " <> show n <> ", " <> show m
