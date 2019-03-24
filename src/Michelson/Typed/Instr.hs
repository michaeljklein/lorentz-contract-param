-- | Module, containing data types for Michelson value.

module Michelson.Typed.Instr
  ( (:+>)(..)
  , (#)
  , type ( & )
  , Instr
  , Contract
  , ExtT
  , InstrExtT
  ) where

import Data.Kind (Type)
import Data.Singletons (SingI)

import Michelson.Typed.Arith
import Michelson.Typed.Polymorphic
import Michelson.Typed.T
  (T, TAddress, TBool, TBytes, TContract, TInt, TKey, TKeyHash, TLambda, TList, TMap, TMutez, TNat,
  TOperation, TOption, TOr, TPair, TSet, TSignature, TTimestamp, TUnit, Tc)

import Michelson.Typed.Value (ContractInp, ContractOut, Value(..))

-- | Infix version of @Seq@ constructor.
--
-- One can represent sequence of Michelson opertaions as follows:
-- @SWAP; DROP; DUP;@ -> @SWAP # DROP # DUP@.
(#) :: Typeable b => (:+>) a b -> (:+>) b c -> (:+>) a c
(#) = Seq

infixr 0 #

type (&) (a :: T) (b :: [T]) = a ': b
infixr 2 &

type Instr = (:+>)

-- | ExtT is extension of Instr by Morley instructions
type family ExtT (instr :: [T] -> [T] -> Type) :: Type

type InstrExtT = ExtT (:+>)

type Value' = Value (:+>)
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

-- pva701: Typeable constraints are added during TM-29.
-- Maybe it makes sense to think how to eliminate them
-- if they break something
data (:+>) (inp :: [T]) (out :: [T]) where
  Seq :: Typeable b => a :+> b -> b :+> c -> a :+> c
  -- | Nop operation. Missing in Michelson spec, added to parse construction
  -- like  `IF {} { SWAP; DROP; }`.
  Nop :: s :+> s

  Ext :: ExtT (:+>) -> s :+> s

  DROP      :: a & s :+> s
  DUP       :: a & s :+> a & a & s
  SWAP      :: a & b & s :+> b & a & s
  PUSH      :: forall t s . SingI t => Value' t -> s :+> t & s
  SOME      :: a & s :+> TOption a & s
  NONE      :: forall a s . SingI a => s :+> TOption a & s
  UNIT      :: s :+> TUnit & s
  IF_NONE   :: (Typeable a, Typeable s)
            => s :+> s' -> a & s :+> s' -> TOption a & s :+> s'
  PAIR      :: a & b & s :+> TPair a b & s
  CAR       :: TPair a b & s :+> a & s
  CDR       :: TPair a b & s :+> b & s
  LEFT      :: forall a b s . SingI b => a & s :+> TOr a b & s
  RIGHT     :: forall a b s . SingI a => b & s :+> TOr a b & s
  IF_LEFT   :: (Typeable s, Typeable a, Typeable b)
            => a & s :+> s' -> b & s :+> s' -> TOr a b & s :+> s'
  IF_RIGHT  :: (Typeable s, Typeable b, Typeable a)
            => b & s :+> s' -> a & s :+> s' -> TOr a b & s :+> s'
  NIL       :: SingI p => s :+> TList p & s
  CONS      :: a & TList a & s :+> TList a & s
  IF_CONS   :: (Typeable s, Typeable a)
            => a & TList a & s :+> s' -> s :+> s' -> TList a & s :+> s'
  SIZE      :: SizeOp c => c & s :+> TNat & s
  EMPTY_SET :: SingI e => s :+> TSet e & s
  EMPTY_MAP :: (SingI a, SingI b) => s :+> TMap a b & s
  MAP       :: (Typeable (MapOpInp c & s), MapOp c b)
            => MapOpInp c & s :+> b & s -> c & s :+> MapOpRes c b & s
  ITER      :: (Typeable (IterOpEl c & s), IterOp c)
            => IterOpEl c & s :+> s -> c & s :+> s
  MEM       :: MemOp c => Tc (MemOpKey c) & c & s :+> TBool & s
  GET       :: GetOp c => Tc (GetOpKey c) & c & s :+> TOption (GetOpValue c) & s
  UPDATE    :: UpdOp c => Tc (UpdOpKey c) & UpdOpParams c & c & s :+> c & s
  IF        :: Typeable s => s :+> s' -> s :+> s' -> TBool & s :+> s'
  LOOP      :: Typeable s => s :+> TBool & s -> TBool & s :+> s
  LOOP_LEFT :: (Typeable a, Typeable s)
            => a & s :+> TOr a b & s -> TOr a b & s :+> b & s
  LAMBDA    :: forall i o s . (SingI i, SingI o)
            => Value' (TLambda i o) -> s :+> TLambda i o & s
  EXEC      :: Typeable a => a & TLambda a b & s :+> b & s
  DIP       :: Typeable a => a :+> c -> b & a :+> b & c
  FAILWITH  :: a & s :+> t
  CAST      :: forall a s . SingI a => (:+>) (a & s) (a & s)
  RENAME    :: (:+>) (a & s) (a & s)
  PACK      :: (:+>) (a & s) (TBytes & s)
  UNPACK    :: SingI a => (:+>) (TBytes & s) (TOption a & s)
  CONCAT    :: ConcatOp c => (:+>) (c & c & s) (c & s)
  CONCAT'   :: ConcatOp c => (:+>) (TList c & s) (c & s)
  SLICE     :: SliceOp c => (:+>) (TNat & TNat & c & s) (TOption c & s)
  ISNAT     :: (:+>) (TInt & s) (TOption (TNat) & s)
  ADD       :: ArithOp Add n m => Tc n & Tc m & s :+> Tc (ArithRes Add n m) & s
  SUB       :: ArithOp Sub n m => Tc n & Tc m & s :+> Tc (ArithRes Sub n m) & s
  MUL       :: ArithOp Mul n m => Tc n & Tc m & s :+> Tc (ArithRes Mul n m) & s
  EDIV      :: EDivOp n m => Tc n & Tc m & s
               :+> TOption (TPair (Tc (EDivOpRes n m)) (Tc (EModOpRes n m))) & s
  ABS       :: UnaryArithOp Abs n => Tc n & s :+> Tc (UnaryArithRes Abs n) & s
  NEG       :: UnaryArithOp Neg n => Tc n & s :+> Tc (UnaryArithRes Neg n) & s
  LSL       :: ArithOp Lsl n m => Tc n & Tc m & s :+> Tc (ArithRes Lsl n m) & s
  LSR       :: ArithOp Lsr n m => Tc n & Tc m & s :+> Tc (ArithRes Lsr n m) & s
  OR        :: ArithOp Or n m  => Tc n & Tc m & s :+> Tc (ArithRes Or n m) & s
  AND       :: ArithOp And n m => Tc n & Tc m & s :+> Tc (ArithRes And n m) & s
  XOR       :: ArithOp Xor n m => Tc n & Tc m & s :+> Tc (ArithRes Xor n m) & s
  NOT       :: UnaryArithOp Not n => Tc n & s :+> Tc (UnaryArithRes Not n) & s
  COMPARE   :: ArithOp Compare n m
            => Tc n & Tc m & s :+> Tc (ArithRes Compare n m) & s
  EQ        :: UnaryArithOp Eq' n => Tc n & s :+> Tc (UnaryArithRes Eq' n) & s
  NEQ       :: UnaryArithOp Neq n => Tc n & s :+> Tc (UnaryArithRes Neq n) & s
  LT        :: UnaryArithOp Lt n => Tc n & s :+> Tc (UnaryArithRes Lt n) & s
  GT        :: UnaryArithOp Gt n => Tc n & s :+> Tc (UnaryArithRes Gt n) & s
  LE        :: UnaryArithOp Le n => Tc n & s :+> Tc (UnaryArithRes Le n) & s
  GE        :: UnaryArithOp Ge n => Tc n & s :+> Tc (UnaryArithRes Ge n) & s
  INT       :: TNat & s :+> TInt & s
  SELF      :: forall (cp :: T) s . s :+> TContract cp & s
  CONTRACT  :: SingI p => TAddress & s :+> TOption (TContract p) & s
  TRANSFER_TOKENS
            :: p & TMutez & TContract p & s :+> TOperation & s
  SET_DELEGATE
            :: TOption (TKeyHash) & s :+> TOperation & s
  CREATE_ACCOUNT
            :: TKeyHash & TOption (TKeyHash) & TBool & TMutez & s
            :+> TOperation & TAddress & s

  CREATE_CONTRACT
            :: (SingI p, SingI g)
            => TKeyHash & TOption (TKeyHash) & TBool & TBool & TMutez
               & TLambda (TPair p g) (TPair (TList TOperation) g)
               & g & s
            :+> TOperation & TAddress & s
  CREATE_CONTRACT2
            :: (SingI p, SingI g)
            => '[ TPair p g ] :+> '[ TPair (TList TOperation) g ]
            -> TKeyHash & TOption (TKeyHash) & TBool & TBool & TMutez & g & s
            :+> TOperation & TAddress & s
  IMPLICIT_ACCOUNT
            :: TKeyHash & s :+> TContract TUnit & s
  NOW       :: s :+> TTimestamp & s
  AMOUNT    :: s :+> TMutez & s
  BALANCE   :: s :+> TMutez & s
  CHECK_SIGNATURE
            :: TKey & TSignature & TBytes & s :+> TBool & s
  SHA256    ::  TBytes & s :+> TBytes & s
  SHA512    ::  TBytes & s :+> TBytes & s
  BLAKE2B   ::  TBytes & s :+> TBytes & s
  HASH_KEY  ::  TKey & s :+> TKeyHash & s
  STEPS_TO_QUOTA
            :: s :+> TNat & s
  SOURCE    ::  s :+> TAddress & s
  SENDER    ::  s :+> TAddress & s
  ADDRESS   :: TContract a & s :+> TAddress & s

infixr 1 :+>

deriving instance Show (ExtT (:+>)) => Show ((:+>) inp out)

type Contract cp st = ContractInp cp st :+> ContractOut st
