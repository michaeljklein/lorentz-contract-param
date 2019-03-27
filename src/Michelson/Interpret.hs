{-# LANGUAGE DerivingStrategies, Rank2Types #-}

-- | Module, containing function to interpret Michelson
-- instructions against given context and input stack.
module Michelson.Interpret
  ( ContractEnv (..)
  , InterpreterEnv (..)
  , InterpreterState (..)
  , MichelsonFailed (..)
  , RemainingSteps (..)
  , SomeItStack (..)
  , EvalOp

  , interpret
  , ContractReturn

  , interpretUntyped
  , InterpretUntypedError (..)
  , InterpretUntypedResult (..)
  , runInstr
  , runInstrNoGas
  ) where

import Prelude hiding (EQ, GT, LT)

import Control.Monad.Except (throwError)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Singletons (SingI(..))
import Data.Typeable ((:~:)(..))
import Data.Vinyl (Rec(..), (<+>))
import Fmt (Buildable(build), Builder, genericF)

import Michelson.TypeCheck
  (ExtC, SomeContract(..), SomeValue(..), TCError, TcExtHandler, eqT', runTypeCheckT,
  typeCheckContract, typeCheckValue)
import Michelson.Typed
  (CValue(..), Contract, ConversibleExt, CreateAccount(..), CreateContract(..), Instr(..),
  Operation(..), SetDelegate(..), Sing(..), T(..), TransferTokens(..), fromUType,
  valToOpOrValue)
import qualified Michelson.Typed as T
import Michelson.Typed.Arith
import Michelson.Typed.Convert (convertContract, unsafeValToValue)
import Michelson.Typed.Polymorphic
import qualified Michelson.Untyped as U
import Tezos.Address (Address(..))
import Tezos.Core (Mutez, Timestamp(..))
import Tezos.Crypto (KeyHash, blake2b, checkSignature, hashKey, sha256, sha512)

-- | Environment for contract execution.
data ContractEnv = ContractEnv
  { ceNow :: !Timestamp
  -- ^ Timestamp of the block whose validation triggered this execution.
  , ceMaxSteps :: !RemainingSteps
  -- ^ Number of steps after which execution unconditionally terminates.
  , ceBalance :: !Mutez
  -- ^ Current amount of mutez of the current contract.
  , ceContracts :: Map Address U.Contract
  -- ^ Mapping from existing contracts' addresses to their executable
  -- representation.
  , ceSelf :: !Address
  -- ^ Address of the interpreted contract.
  , ceSource :: !Address
  -- ^ The contract that initiated the current transaction.
  , ceSender :: !Address
  -- ^ The contract that initiated the current internal transaction.
  , ceAmount :: !Mutez
  -- ^ Amount of the current transaction.
  }

-- | Represents `[FAILED]` state of a Michelson program. Contains
-- value that was on top of the stack when `FAILWITH` was called.
data MichelsonFailed where
  MichelsonFailedWith :: T.Value t -> MichelsonFailed
  MichelsonArithError :: ArithError (CValue n) (CValue m) -> MichelsonFailed
  MichelsonGasExhaustion :: MichelsonFailed
  MichelsonFailedOther :: Text -> MichelsonFailed

deriving instance Show MichelsonFailed

instance (ConversibleExt, Buildable U.Instr) => Buildable MichelsonFailed where
  build =
    \case
      MichelsonFailedWith (v :: T.Value t) ->
        "Reached FAILWITH instruction with " <> formatValue v
      MichelsonArithError v -> build v
      MichelsonGasExhaustion ->
        "Gas limit exceeded on contract execution"
      MichelsonFailedOther t -> build t
    where
      formatValue :: forall t . T.Value t -> Builder
      formatValue v =
        case valToOpOrValue v of
          Nothing -> "<value with operations>"
          Just untypedV -> build untypedV

data InterpretUntypedError s
  = RuntimeFailure (MichelsonFailed, s)
  | IllTypedContract TCError
  | IllTypedParam TCError
  | IllTypedStorage TCError
  | UnexpectedParamType Text
  | UnexpectedStorageType Text
  deriving (Generic)

deriving instance (Buildable U.Instr, Show s) => Show (InterpretUntypedError s)

instance (ConversibleExt, Buildable U.Instr, Buildable s) => Buildable (InterpretUntypedError s) where
  build = genericF

data InterpretUntypedResult s where
  InterpretUntypedResult
    :: ( Typeable st
       , SingI st
       )
    => { iurOps :: [ Operation Instr ]
       , iurNewStorage :: T.Value st
       , iurNewState   :: InterpreterState s
       }
    -> InterpretUntypedResult s

-- | Interpret a contract without performing any side effects.
interpretUntyped
  :: forall s . (ExtC, Aeson.ToJSON U.InstrExtU)
  => TcExtHandler
  -> U.Contract
  -> U.Value
  -> U.Value
  -> InterpreterEnv s
  -> s
  -> Either (InterpretUntypedError s) (InterpretUntypedResult s)
interpretUntyped typeCheckHandler U.Contract{..} paramU initStU env initState = do
    (SomeContract (instr :: Contract cp st) _ _)
       <- first IllTypedContract $ typeCheckContract typeCheckHandler
              (U.Contract para stor (U.unOp <$> code))
    paramV :::: ((_ :: Sing cp1), _)
       <- first IllTypedParam $ runTypeCheckT typeCheckHandler para $
            typeCheckValue paramU (fromUType para)
    initStV :::: ((_ :: Sing st1), _)
       <- first IllTypedStorage $ runTypeCheckT typeCheckHandler para $
            typeCheckValue initStU (fromUType stor)
    Refl <- first UnexpectedStorageType $ eqT' @st @st1
    Refl <- first UnexpectedParamType   $ eqT' @cp @cp1
    bimap RuntimeFailure constructIUR $
      toRes $ interpret instr paramV initStV env initState
  where
    toRes (ei, s) = bimap (,isExtState s) (,s) ei

    constructIUR ::
      (Typeable st, SingI st) =>
      (([Operation Instr], T.Value st), InterpreterState s) ->
      InterpretUntypedResult s
    constructIUR ((ops, val), st) =
      InterpretUntypedResult
      { iurOps = ops
      , iurNewStorage = val
      , iurNewState = st
      }

type ContractReturn s st =
  (Either MichelsonFailed ([Operation Instr], T.Value st), InterpreterState s)

interpret
  :: (ExtC, Aeson.ToJSON U.InstrExtU, Typeable cp, Typeable st)
  => Contract cp st
  -> T.Value cp
  -> T.Value st
  -> InterpreterEnv s
  -> s
  -> ContractReturn s st
interpret instr param initSt env@InterpreterEnv{..} initState = first (fmap toRes) $
  runEvalOp
    (runInstr instr (T.VPair (param, initSt) :& RNil))
    env
    (InterpreterState initState $ ceMaxSteps ieContractEnv)
  where
    toRes
      :: (Rec (T.Value' instr) '[ 'TPair ('TList 'TOperation) st ])
      -> ([Operation instr], T.Value' instr st)
    toRes (T.VPair (T.VList ops_, newSt) :& RNil) =
      (map (\(T.VOp op) -> op) ops_, newSt)

data SomeItStack where
  SomeItStack :: Typeable inp => Rec (T.Value) inp -> SomeItStack

data InterpreterEnv s = InterpreterEnv
  { ieContractEnv :: ContractEnv
  , ieItHandler   :: (T.InstrExtT, SomeItStack) -> EvalOp s ()
  }

newtype RemainingSteps = RemainingSteps Word64
  deriving stock (Show)
  deriving newtype (Eq, Ord, Buildable, Num)

data InterpreterState s = InterpreterState
  { isExtState       :: s
  , isRemainingSteps :: RemainingSteps
  } deriving (Show)

type EvalOp s a =
  ExceptT MichelsonFailed
    (ReaderT (InterpreterEnv s)
       (State (InterpreterState s))) a

runEvalOp ::
     EvalOp s a
  -> InterpreterEnv s
  -> InterpreterState s
  -> (Either MichelsonFailed a, InterpreterState s)
runEvalOp act env initSt =
  flip runState initSt $ usingReaderT env $ runExceptT act

-- | Function to change amount of remaining steps stored in State monad
runInstr
  :: (ExtC, Aeson.ToJSON U.InstrExtU, Typeable inp)
  => Instr inp out
  -> Rec (T.Value) inp
  -> EvalOp state (Rec (T.Value) out)
runInstr i@(Seq _i1 _i2) r = runInstrImpl runInstr i r
runInstr i@Nop r = runInstrImpl runInstr i r
runInstr i r = do
  rs <- gets isRemainingSteps
  if rs == 0
  then throwError $ MichelsonGasExhaustion
  else do
    modify (\s -> s {isRemainingSteps = rs - 1})
    runInstrImpl runInstr i r

runInstrNoGas
  :: forall a b state .
  (ExtC, Aeson.ToJSON U.InstrExtU, Typeable a)
  => T.Instr a b -> Rec T.Value a -> EvalOp state (Rec T.Value b)
runInstrNoGas = runInstrImpl runInstrNoGas

-- | Function to interpret Michelson instruction(s) against given stack.
runInstrImpl
    :: forall state .
    (ExtC, Aeson.ToJSON U.InstrExtU)
    => (forall inp1 out1 . Typeable inp1 =>
      Instr inp1 out1
    -> Rec (T.Value) inp1
    -> EvalOp state (Rec T.Value out1)
    )
    -> (forall inp out . Typeable inp =>
      Instr inp out
    -> Rec (T.Value) inp
    -> EvalOp state (Rec T.Value out)
    )
runInstrImpl runner (Seq i1 i2) r = runner i1 r >>= \r' -> runner i2 r'
runInstrImpl _ Nop r = pure $ r
runInstrImpl _ (Ext nop) r = do
  handler <- asks ieItHandler
  r <$ handler (nop, SomeItStack r)
runInstrImpl _ DROP (_ :& r) = pure $ r
runInstrImpl _ DUP (a :& r) = pure $ a :& a :& r
runInstrImpl _ SWAP (a :& b :& r) = pure $ b :& a :& r
runInstrImpl _ (PUSH v) r = pure $ v :& r
runInstrImpl _ SOME (a :& r) = pure $ T.VOption (Just a) :& r
runInstrImpl _ NONE r = pure $ T.VOption Nothing :& r
runInstrImpl _ UNIT r = pure $ T.VUnit :& r
runInstrImpl runner (IF_NONE _bNone bJust) (T.VOption (Just a) :& r) = runner bJust (a :& r)
runInstrImpl runner (IF_NONE bNone _bJust) (T.VOption Nothing :& r) = runner bNone r
runInstrImpl _ PAIR (a :& b :& r) = pure $ T.VPair (a, b) :& r
runInstrImpl _ CAR (T.VPair (a, _b) :& r) = pure $ a :& r
runInstrImpl _ CDR (T.VPair (_a, b) :& r) = pure $ b :& r
runInstrImpl _ LEFT (a :& r) = pure $ (T.VOr $ Left a) :& r
runInstrImpl _ RIGHT (b :& r) = pure $ (T.VOr $ Right b) :& r
runInstrImpl runner (IF_LEFT bLeft _) (T.VOr (Left a) :& r) = runner bLeft (a :& r)
runInstrImpl runner (IF_LEFT _ bRight) (T.VOr (Right a) :& r) = runner bRight (a :& r)
runInstrImpl runner (IF_RIGHT bRight _) (T.VOr (Right a) :& r) = runner bRight (a :& r)
runInstrImpl runner (IF_RIGHT _ bLeft) (T.VOr (Left a) :& r) = runner bLeft (a :& r)
-- More here
runInstrImpl _ NIL r = pure $ T.VList [] :& r
runInstrImpl _ CONS (a :& T.VList l :& r) = pure $ T.VList (a : l) :& r
runInstrImpl runner (IF_CONS _ bNil) (T.VList [] :& r) = runner bNil r
runInstrImpl runner (IF_CONS bCons _) (T.VList (lh : lr) :& r) = runner bCons (lh :& T.VList lr :& r)
runInstrImpl _ SIZE (a :& r) = pure $ T.VC (CvNat $ (fromInteger . toInteger) $ evalSize a) :& r
runInstrImpl _ EMPTY_SET r = pure $ T.VSet Set.empty :& r
runInstrImpl _ EMPTY_MAP r = pure $ T.VMap Map.empty :& r
runInstrImpl runner (MAP ops) (a :& r) =
  case ops of
    (code :: Instr (MapOpInp c ': s) (b ': s)) -> do
      newList <- mapM (\(val :: T.Value (MapOpInp c)) -> do
        res <- runner code (val :& r)
        case res of
          ((newVal :: T.Value b) :& _) -> pure newVal)
        $ mapOpToList @c @b a
      pure $ mapOpFromList a newList :& r
runInstrImpl runner (ITER ops) (a :& r) =
  case ops of
    (code :: Instr (IterOpEl c ': s) s) ->
      case iterOpDetachOne @c a of
        (Just x, xs) -> do
          res <- runner code (x :& r)
          runner (ITER code) (xs :& res)
        (Nothing, _) -> pure r
runInstrImpl _ MEM (T.VC a :& b :& r) = pure $ T.VC (CvBool (evalMem a b)) :& r
runInstrImpl _ GET (T.VC a :& b :& r) = pure $ T.VOption (evalGet a b) :& r
runInstrImpl _ UPDATE (T.VC a :& b :& c :& r) = pure $ evalUpd a b c :& r
runInstrImpl runner (IF bTrue _) (T.VC (CvBool True) :& r) = runner bTrue r
runInstrImpl runner (IF _ bFalse) (T.VC (CvBool False) :& r) = runner bFalse r
runInstrImpl _ (LOOP _) (T.VC (CvBool False) :& r) = pure $ r
runInstrImpl runner (LOOP ops) (T.VC (CvBool True) :& r) = do
  res <- runner ops r
  runner (LOOP ops) res
runInstrImpl _ (LOOP_LEFT _) (T.VOr (Right a) :&r) = pure $ a :& r
runInstrImpl runner (LOOP_LEFT ops) (T.VOr (Left a) :& r) = do
  res <- runner ops (a :& r)
  runner  (LOOP_LEFT ops) res
runInstrImpl _ (LAMBDA lam) r = pure $ lam :& r
runInstrImpl runner EXEC (a :& T.VLam lBody :& r) = do
  res <- runner lBody (a :& RNil)
  pure $ res <+> r
runInstrImpl runner (DIP i) (a :& r) = do
  res <- runner i r
  pure $ a :& res
runInstrImpl _ FAILWITH (a :& _) = throwError $ MichelsonFailedWith a
runInstrImpl _ CAST (a :& r) = pure $ a :& r
runInstrImpl _ RENAME (a :& r) = pure $ a :& r
-- TODO
runInstrImpl _ PACK (_ :& _) = error "PACK not implemented yet"
runInstrImpl _ UNPACK (_ :& _) = error "UNPACK not implemented yet"
runInstrImpl _ CONCAT (a :& b :& r) = pure $ evalConcat a b :& r
runInstrImpl _ CONCAT' (T.VList a :& r) = pure $ evalConcat' a :& r
runInstrImpl _ SLICE (T.VC (CvNat o) :& T.VC (CvNat l) :& s :& r) =
  pure $ T.VOption (evalSlice o l s) :& r
runInstrImpl _ ISNAT (T.VC (CvInt i) :& r) =
  if i < 0
  then pure $ T.VOption Nothing :& r
  else pure $ T.VOption (Just $ T.VC (CvNat $ fromInteger i)) :& r
runInstrImpl _ ADD (T.VC l :& T.VC r :& rest) =
  (:& rest) <$> runArithOp (Proxy @Add) l r
runInstrImpl _ SUB (T.VC l :& T.VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Sub) l r
runInstrImpl _ MUL (T.VC l :& T.VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Mul) l r
runInstrImpl _ EDIV (T.VC l :& T.VC r :& rest) = pure $ evalEDivOp l r :& rest
runInstrImpl _ ABS (T.VC a :& rest) = pure $ T.VC (evalUnaryArithOp (Proxy @Abs) a) :& rest
runInstrImpl _ NEG (T.VC a :& rest) = pure $ T.VC (evalUnaryArithOp (Proxy @Neg) a) :& rest
runInstrImpl _ LSL (T.VC x :& T.VC s :& rest) = (:& rest) <$> runArithOp (Proxy @Lsl) x s
runInstrImpl _ LSR (T.VC x :& T.VC s :& rest) = (:& rest) <$> runArithOp (Proxy @Lsr) x s
runInstrImpl _ OR (T.VC l :& T.VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Or) l r
runInstrImpl _ AND (T.VC l :& T.VC r :& rest) = (:& rest) <$> runArithOp (Proxy @And) l r
runInstrImpl _ XOR (T.VC l :& T.VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Xor) l r
runInstrImpl _ NOT (T.VC a :& rest) = pure $ T.VC (evalUnaryArithOp (Proxy @Not) a) :& rest
runInstrImpl _ COMPARE (T.VC l :& T.VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Compare) l r
runInstrImpl _ T.EQ (T.VC a :& rest) = pure $ T.VC (evalUnaryArithOp (Proxy @Eq') a) :& rest
runInstrImpl _ NEQ (T.VC a :& rest) = pure $ T.VC (evalUnaryArithOp (Proxy @Neq) a) :& rest
runInstrImpl _ T.LT (T.VC a :& rest) = pure $ T.VC (evalUnaryArithOp (Proxy @Lt) a) :& rest
runInstrImpl _ T.GT (T.VC a :& rest) = pure $ T.VC (evalUnaryArithOp (Proxy @Gt) a) :& rest
runInstrImpl _ LE (T.VC a :& rest) = pure $ T.VC (evalUnaryArithOp (Proxy @Le) a) :& rest
runInstrImpl _ GE (T.VC a :& rest) = pure $ T.VC (evalUnaryArithOp (Proxy @Ge) a) :& rest
runInstrImpl _ INT (T.VC (CvNat n) :& r) = pure $ T.VC (CvInt $ toInteger n) :& r
runInstrImpl _ SELF r = do
  ContractEnv{..} <- asks ieContractEnv
  pure $ T.VContract ceSelf :& r
runInstrImpl _ CONTRACT (T.VC (CvAddress addr) :& r) = do
  ContractEnv{..} <- asks ieContractEnv
  if Map.member addr ceContracts
  then pure $ T.VOption (Just $ T.VContract addr) :& r
  else pure $ T.VOption Nothing :& r
runInstrImpl _ TRANSFER_TOKENS (p :& T.VC (CvMutez mutez) :& contract :& r) =
  pure $ T.VOp (OpTransferTokens $ TransferTokens p mutez contract) :& r
runInstrImpl _ SET_DELEGATE (T.VOption mbKeyHash :& r) =
  case mbKeyHash of
    Just (T.VC (CvKeyHash k)) -> pure $ T.VOp (OpSetDelegate $ SetDelegate $ Just k) :& r
    Nothing -> pure $ T.VOp (OpSetDelegate $ SetDelegate $ Nothing) :& r
runInstrImpl _ CREATE_ACCOUNT
  (T.VC (CvKeyHash k) :& T.VOption mbKeyHash :&
    (T.VC (CvBool spendable)) :& (T.VC (CvMutez m)) :& r) =
  pure (T.VOp (OpCreateAccount $ CreateAccount k (unwrapMbKeyHash mbKeyHash) spendable m)
    :& (T.VC . CvAddress) (KeyAddress k) :& r)
runInstrImpl _ CREATE_CONTRACT
  (T.VC (CvKeyHash k) :& T.VOption mbKeyHash :& (T.VC (CvBool spendable)) :&
    (T.VC (CvBool delegetable)) :& (T.VC (CvMutez m)) :& T.VLam ops :& g :& r) =
  pure (T.VOp (OpCreateContract $
    CreateContract k (unwrapMbKeyHash mbKeyHash) spendable delegetable m g ops)
    :& (T.VC . CvAddress) (U.mkContractAddress $
      createOrigOp k mbKeyHash spendable delegetable m ops g) :& r)
runInstrImpl _ (CREATE_CONTRACT2 ops)
  (T.VC (CvKeyHash k) :& T.VOption mbKeyHash :& (T.VC (CvBool spendable)) :&
    (T.VC (CvBool delegetable)) :& (T.VC (CvMutez m)) :& g :& r) =
  pure (T.VOp (OpCreateContract $
    CreateContract k (unwrapMbKeyHash mbKeyHash) spendable delegetable m g ops)
    :& (T.VC . CvAddress) (U.mkContractAddress $
      createOrigOp k mbKeyHash spendable delegetable m ops g) :& r)
runInstrImpl _ IMPLICIT_ACCOUNT (T.VC (CvKeyHash k) :& r) =
  pure $ T.VContract (KeyAddress k) :& r
runInstrImpl _ NOW r = do
  ContractEnv{..} <- asks ieContractEnv
  pure $ T.VC (CvTimestamp ceNow) :& r
runInstrImpl _ AMOUNT r = do
  ContractEnv{..} <- asks ieContractEnv
  pure $ T.VC (CvMutez ceAmount) :& r
runInstrImpl _ BALANCE r = do
  ContractEnv{..} <- asks ieContractEnv
  pure $ T.VC (CvMutez ceBalance) :& r
runInstrImpl _ CHECK_SIGNATURE (T.VKey k :& T.VSignature v :&
  T.VC (CvBytes b) :& r) = pure $ T.VC (CvBool $ checkSignature k v b) :& r
runInstrImpl _ SHA256 (T.VC (CvBytes b) :& r) = pure $ T.VC (CvBytes $ sha256 b) :& r
runInstrImpl _ SHA512 (T.VC (CvBytes b) :& r) = pure $ T.VC (CvBytes $ sha512 b) :& r
runInstrImpl _ BLAKE2B (T.VC (CvBytes b) :& r) = pure $ T.VC (CvBytes $ blake2b b) :& r
runInstrImpl _ HASH_KEY (T.VKey k :& r) = pure $ T.VC (CvKeyHash $ hashKey k) :& r
runInstrImpl _ STEPS_TO_QUOTA r = do
  RemainingSteps x <- gets isRemainingSteps
  pure $ T.VC (CvNat $ (fromInteger . toInteger) x) :& r
runInstrImpl _ SOURCE r = do
  ContractEnv{..} <- asks ieContractEnv
  pure $ T.VC (CvAddress ceSource) :& r
runInstrImpl _ SENDER r = do
  ContractEnv{..} <- asks ieContractEnv
  pure $ T.VC (CvAddress ceSender) :& r
runInstrImpl _ ADDRESS (T.VContract a :& r) = pure $ T.VC (CvAddress a) :& r

-- | Evaluates an arithmetic operation and either fails or proceeds.
runArithOp
  :: ArithOp aop n m
  => proxy aop
  -> CValue n
  -> CValue m
  -> EvalOp s (T.Value' instr ('Tc (ArithRes aop n m)))
runArithOp op l r = case evalOp op l r of
  Left  err -> throwError (MichelsonArithError err)
  Right res -> pure (T.VC res)

createOrigOp
  :: (SingI param, SingI store, ConversibleExt)
  => KeyHash
  -> Maybe (T.Value ('Tc 'U.CKeyHash))
  -> Bool -> Bool -> Mutez
  -> Contract param store
  -> T.Value t
  -> U.OriginationOperation
createOrigOp k mbKeyHash delegetable spendable m contract g =
  U.OriginationOperation
    { ooManager = k
    , ooDelegate = (unwrapMbKeyHash mbKeyHash)
    , ooSpendable = spendable
    , ooDelegatable = delegetable
    , ooBalance = m
    , ooStorage = unsafeValToValue g
    , ooContract = convertContract contract
    }

unwrapMbKeyHash :: Maybe (T.Value ('Tc 'U.CKeyHash)) -> Maybe KeyHash
unwrapMbKeyHash (Just (T.VC (CvKeyHash keyHash))) = Just keyHash
unwrapMbKeyHash Nothing = Nothing
