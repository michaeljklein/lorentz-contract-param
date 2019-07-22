{-# LANGUAGE DerivingStrategies, Rank2Types #-}

-- | Module, containing function to interpret Michelson
-- instructions against given context and input stack.
module Michelson.Interpret
  ( ContractEnv (..)
  , InterpreterState (..)
  , MichelsonFailed (..)
  , RemainingSteps (..)
  , SomeItStack (..)
  , EvalOp
  , MorleyLogs (..)
  , noMorleyLogs

  , interpret
  , interpretRepeated
  , interpretInstr
  , ContractReturn

  , interpretUntyped
  , InterpretUntypedError (..)
  , InterpretUntypedResult (..)
  , runInstr
  , runInstrNoGas
  , runUnpack
  ) where

import Prelude hiding (EQ, GT, LT)

import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import Data.Default (Default (..))
import qualified Data.Set as Set
import Data.Singletons (SingI(..))
import Data.Typeable ((:~:)(..))
import Data.Vinyl (Rec(..), (<+>))
import Fmt (Buildable(build), Builder, genericF)
import Michelson.EqParam (eqParam1, eqParam2)

import Gas.Type (Cost, free)
import qualified Gas.Cost.CostOf as Gas
import Michelson.Interpret.Pack (packValue')
import Michelson.Interpret.Unpack (UnpackError, unpackValue', UnpackEnv (..))
import Michelson.TypeCheck
  ( SomeContract(..), SomeNotedValue(..), TCError, TcOriginatedContracts,
  TCTypeError(..), compareTypes, eqType, runTypeCheck, typeCheckContract, typeCheckValue)
import Michelson.Typed
  (CValue(..), Contract, CreateAccount(..), CreateContract(..), HasNoBigMap, HasNoOp, Instr(..),
  OpPresence(..), Operation'(..), Operation, SetDelegate(..), Sing(..), T(..), TransferTokens(..), Value'(..),
  extractNotes, fromUType, withSomeSingT)
import qualified Michelson.Typed as T
import Michelson.Typed.Arith
import Michelson.Typed.Convert (convertContract, untypeValue)
import Michelson.Typed.Polymorphic
import qualified Michelson.Untyped as U
import Util.Peano (Peano, LongerThan, Sing(SS, SZ))
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
  , ceContracts :: TcOriginatedContracts
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
  MichelsonFailedWith :: (Typeable t, SingI t) => T.Value t -> MichelsonFailed
  MichelsonArithError :: (Typeable n, Typeable m) => ArithError (CValue n) (CValue m) -> MichelsonFailed
  MichelsonGasExhaustion :: MichelsonFailed
  MichelsonFailedTestAssert :: Text -> MichelsonFailed

deriving instance Show MichelsonFailed

instance Eq MichelsonFailed where
  MichelsonFailedWith v1 == MichelsonFailedWith v2 = v1 `eqParam1` v2
  MichelsonFailedWith _ == _ = False
  MichelsonArithError ae1 == MichelsonArithError ae2 = ae1 `eqParam2` ae2
  MichelsonArithError _ == _ = False
  MichelsonGasExhaustion == MichelsonGasExhaustion = True
  MichelsonGasExhaustion == _ = False
  MichelsonFailedTestAssert t1 == MichelsonFailedTestAssert t2 = t1 == t2
  MichelsonFailedTestAssert _ == _ = False

instance Buildable MichelsonFailed where
  build =
    \case
      MichelsonFailedWith (v :: T.Value t) ->
        "Reached FAILWITH instruction with " <> formatValue v
      MichelsonArithError v -> build v
      MichelsonGasExhaustion ->
        "Gas limit exceeded on contract execution"
      MichelsonFailedTestAssert t -> build t
    where
      formatValue :: forall t . SingI t => Value' Instr t -> Builder
      formatValue v =
        case T.checkOpPresence (sing @t) of
          OpPresent -> "<value with operations>"
          OpAbsent -> build (untypeValue v)

data InterpretUntypedError
  = RuntimeFailure (MichelsonFailed, MorleyLogs)
  | IllTypedContract TCError
  | IllTypedParam TCError
  | IllTypedStorage TCError
  | UnexpectedParamType TCTypeError
  | UnexpectedStorageType TCTypeError
  deriving (Generic)

deriving instance Show InterpretUntypedError

instance Buildable InterpretUntypedError where
  build = genericF

data InterpretUntypedResult where
  InterpretUntypedResult
    :: ( Typeable st
       , SingI st
       , HasNoOp st
       )
    => { iurOps :: [Operation]
       , iurNewStorage :: T.Value st
       , iurNewState   :: InterpreterState
       }
    -> InterpretUntypedResult

deriving instance Show InterpretUntypedResult

-- | Morley interpreter state
newtype MorleyLogs = MorleyLogs
  { unMorleyLogs :: [Text]
  } deriving stock (Eq, Show)
    deriving newtype (Default, Buildable)

noMorleyLogs :: MorleyLogs
noMorleyLogs = MorleyLogs []

-- | Interpret a contract without performing any side effects.
interpretUntyped
  :: U.Contract
  -> U.Value
  -> U.Value
  -> ContractEnv
  -> Either InterpretUntypedError InterpretUntypedResult
interpretUntyped U.Contract{..} paramU initStU env = do
  (SomeContract (instr :: Contract cp st) _ _)
      <- first IllTypedContract $ typeCheckContract (ceContracts env)
            (U.Contract para stor code)
  withSomeSingT (fromUType para) $ \sgp ->
    withSomeSingT (fromUType stor) $ \sgs -> do
      ntp <- first (UnexpectedParamType . ExtractionTypeMismatch) $ extractNotes para sgp
      nts <- first (UnexpectedStorageType . ExtractionTypeMismatch) $ extractNotes stor sgs
      paramV :::: ((_ :: Sing cp1), _)
          <- first IllTypedParam $ runTypeCheck para (ceContracts env) $ usingReaderT def $
               typeCheckValue paramU (sgp, ntp)
      initStV :::: ((_ :: Sing st1), _)
          <- first IllTypedStorage $ runTypeCheck para (ceContracts env) $ usingReaderT def $
               typeCheckValue initStU (sgs, nts)
      Refl <- first UnexpectedStorageType $ eqType @st @st1
      Refl <- first UnexpectedParamType   $ eqType @cp @cp1
      bimap RuntimeFailure constructIUR $
        toRes $ interpret instr paramV initStV env
  where
    toRes (ei, s) = bimap (,isMorleyLogs s) (,s) ei

    constructIUR ::
      (Typeable st, SingI st, HasNoOp st) =>
      (([Operation], Value' Instr st), InterpreterState) ->
      InterpretUntypedResult
    constructIUR ((ops, val), st) =
      InterpretUntypedResult
      { iurOps = ops
      , iurNewStorage = val
      , iurNewState = st
      }

type ContractReturn st =
  (Either MichelsonFailed ([Operation], T.Value st), InterpreterState)

interpret'
  :: Contract cp st
  -> T.Value cp
  -> T.Value st
  -> ContractEnv
  -> InterpreterState
  -> ContractReturn st
interpret' instr param initSt env ist = first (fmap toRes) $
  runEvalOp
    (runInstr instr (T.VPair (param, initSt) :& RNil))
    env
    ist
  where
    toRes
      :: (Rec (T.Value' instr) '[ 'TPair ('TList 'TOperation) st ])
      -> ([T.Operation' instr], T.Value' instr st)
    toRes (T.VPair (T.VList ops_, newSt) :& RNil) =
      (map (\(T.VOp op) -> op) ops_, newSt)

interpret
  :: Contract cp st
  -> T.Value cp
  -> T.Value st
  -> ContractEnv
  -> ContractReturn st
interpret instr param initSt env =
  interpret' instr param initSt env (InterpreterState def (ceMaxSteps env) free)

-- | Emulate multiple calls of a contract.
interpretRepeated
  :: Contract cp st
  -> [T.Value cp]
  -> T.Value st
  -> ContractEnv
  -> ContractReturn st
interpretRepeated instr params initSt env =
  foldl interpretDo
    (Right ([], initSt), (InterpreterState def (ceMaxSteps env) free))
    params
  where
    interpretDo (!res, !ist) param =
      case res of
        Right (ops, st) ->
          let (res2, ist2) = interpret' instr param st env ist
          in (res2 <&> \(ops2, st2) -> (ops ++ ops2, st2), ist2)
        Left err ->
          (Left err, ist)

-- | Interpret an instruction in vacuum, putting no extra contraints on
-- its execution.
--
-- Mostly for testing purposes.
interpretInstr
  :: ContractEnv
  -> Instr inp out
  -> Rec T.Value inp
  -> Either MichelsonFailed (Rec T.Value out)
interpretInstr env instr inpSt =
  fst $
  runEvalOp
    (runInstr instr inpSt)
    env
    InterpreterState { isMorleyLogs = MorleyLogs []
                     , isRemainingSteps = 9999999999
                     , isCost = free
                     }

data SomeItStack where
  SomeItStack :: T.ExtInstr inp -> Rec T.Value inp -> SomeItStack

newtype RemainingSteps = RemainingSteps Word64
  deriving stock (Show)
  deriving newtype (Eq, Ord, Buildable, Num)

data InterpreterState = InterpreterState
  { isMorleyLogs     :: MorleyLogs
  , isRemainingSteps :: RemainingSteps
  , isCost :: Cost
  } deriving (Show)

type EvalOp a =
  ExceptT MichelsonFailed
    (ReaderT ContractEnv
       (State InterpreterState)) a

consume :: Cost -> EvalOp ()
consume cost = do
  oldCost <- gets isCost
  modify (\s -> s {isCost = oldCost <> cost})

runEvalOp ::
     EvalOp a
  -> ContractEnv
  -> InterpreterState
  -> (Either MichelsonFailed a, InterpreterState)
runEvalOp act env initSt =
  flip runState initSt $ usingReaderT env $ runExceptT act

-- | Function to change amount of remaining steps stored in State monad
runInstr
  :: Instr inp out
  -> Rec (T.Value) inp
  -> EvalOp (Rec (T.Value) out)
runInstr i@(Seq _i1 _i2) r = runInstrImpl runInstr i r
runInstr i@Nop r = runInstrImpl runInstr i r
runInstr i@(Nested _) r = runInstrImpl runInstr i r
runInstr i r = do
  rs <- gets isRemainingSteps
  if rs == 0
  then throwError $ MichelsonGasExhaustion
  else do
    modify (\s -> s {isRemainingSteps = rs - 1})
    runInstrImpl runInstr i r

runInstrNoGas
  :: forall a b . T.Instr a b -> Rec T.Value a -> EvalOp (Rec T.Value b)
runInstrNoGas = runInstrImpl runInstrNoGas

-- | Function to interpret Michelson instruction(s) against given stack.
runInstrImpl
    :: (forall inp1 out1 .
           Instr inp1 out1
        -> Rec (T.Value) inp1
        -> EvalOp (Rec T.Value out1)
    ) ->
       (forall inp out .
           Instr inp out
        -> Rec (T.Value) inp
        -> EvalOp (Rec T.Value out)
      )
runInstrImpl runner (Seq i1 i2) r = runner i1 r >>= \r' -> runner i2 r'
runInstrImpl _ Nop r = pure $ r
runInstrImpl _ (Ext nop) r = r <$ interpretExt (SomeItStack nop r)
runInstrImpl runner (Nested sq) r = runInstrImpl runner sq r
runInstrImpl _ DROP (_ :& r) = do consume Gas.stackOp; pure $ r
runInstrImpl _ DUP (a :& r) = do consume Gas.stackOp; pure $ a :& a :& r
runInstrImpl _ SWAP (a :& b :& r) = do consume Gas.stackOp; pure $ b :& a :& r
runInstrImpl _ (PUSH v) r = do consume Gas.push; pure $ v :& r
runInstrImpl _ SOME (a :& r) = do consume Gas.wrap; pure $ VOption (Just a) :& r
runInstrImpl _ NONE r = do consume Gas.variantNoData; pure $ VOption Nothing :& r
runInstrImpl _ UNIT r = do consume Gas.push; pure $ VUnit :& r
runInstrImpl runner (IF_NONE _bNone bJust) (VOption (Just a) :& r) =
  do consume Gas.branch; runner bJust (a :& r)
runInstrImpl runner (IF_NONE bNone _bJust) (VOption Nothing :& r) =
  do consume Gas.branch; runner bNone r
runInstrImpl _ PAIR (a :& b :& r) = do consume Gas.pair; pure $ VPair (a, b) :& r
runInstrImpl _ CAR (VPair (a, _b) :& r) = do consume Gas.pairAccess; pure $ a :& r
runInstrImpl _ CDR (VPair (_a, b) :& r) = do consume Gas.pairAccess; pure $ b :& r
runInstrImpl _ LEFT (a :& r) = do consume Gas.wrap; pure $ (VOr $ Left a) :& r
runInstrImpl _ RIGHT (b :& r) = do consume Gas.wrap; pure $ (VOr $ Right b) :& r
runInstrImpl runner (IF_LEFT bLeft _) (VOr (Left a) :& r) =
  do consume Gas.branch; runner bLeft (a :& r)
runInstrImpl runner (IF_LEFT _ bRight) (VOr (Right a) :& r) =
  do consume Gas.branch; runner bRight (a :& r)
-- More here
runInstrImpl _ NIL r = do consume Gas.variantNoData; pure $ VList [] :& r
runInstrImpl _ CONS (a :& VList l :& r) =
  do consume Gas.cons; pure $ VList (a : l) :& r
runInstrImpl runner (IF_CONS _ bNil) (VList [] :& r) =
  do consume Gas.branch; runner bNil r
runInstrImpl runner (IF_CONS bCons _) (VList (lh : lr) :& r) =
  do consume Gas.branch; runner bCons (lh :& VList lr :& r)
runInstrImpl _ SIZE (a :& r) = do
  consume $ costSize a
  pure $ VC (CvNat $ (fromInteger . toInteger) $ evalSize a) :& r
runInstrImpl _ EMPTY_SET r = do consume Gas.emptySet; pure $ VSet Set.empty :& r
runInstrImpl _ EMPTY_MAP r = do consume Gas.emptyMap; pure $ VMap Map.empty :& r
runInstrImpl runner (MAP ops) (a :& r) = do
  consume $ costMapOp a
  case ops of
    (code :: Instr (MapOpInp c ': s) (b ': s)) -> do
      newList <- mapM (\(val :: T.Value (MapOpInp c)) -> do
        res <- runner code (val :& r)
        case res of
          ((newVal :: T.Value b) :& _) -> pure newVal)
        $ mapOpToList @c a
      pure $ mapOpFromList a newList :& r
runInstrImpl runner (ITER ops) (a :& r) = do
  consume $ costIterOp a
  case ops of
    (code :: Instr (IterOpEl c ': s) s) ->
      case iterOpDetachOne @c a of
        (Just x, xs) -> do
          res <- runner code (x :& r)
          runner (ITER code) (xs :& res)
        (Nothing, _) -> pure r
runInstrImpl _ MEM (VC a :& b :& r) =
  do consume $ costMem b; pure $ VC (CvBool (evalMem a b)) :& r
runInstrImpl _ GET (VC a :& b :& r) =
  do consume $ costGet b; pure $ VOption (evalGet a b) :& r
runInstrImpl _ UPDATE (VC a :& b :& c :& r) =
  do consume $ costUpd c; pure $ evalUpd a b c :& r
runInstrImpl runner (IF bTrue _) (VC (CvBool True) :& r) =
  do consume Gas.branch; runner bTrue r
runInstrImpl runner (IF _ bFalse) (VC (CvBool False) :& r) =
  do consume Gas.branch; runner bFalse r
runInstrImpl _ (LOOP _) (VC (CvBool False) :& r) = pure $ r
runInstrImpl runner (LOOP ops) (VC (CvBool True) :& r) = do
  consume Gas.loopCycle
  res <- runner ops r
  runner (LOOP ops) res
runInstrImpl _ (LOOP_LEFT _) (VOr (Right a) :&r) = pure $ a :& r
runInstrImpl runner (LOOP_LEFT ops) (VOr (Left a) :& r) = do
  consume Gas.loopCycle
  res <- runner ops (a :& r)
  runner  (LOOP_LEFT ops) res
runInstrImpl _ (LAMBDA lam) r = do consume Gas.push; pure $ lam :& r
runInstrImpl runner EXEC (a :& VLam lBody :& r) = do
  consume Gas.exec
  res <- runner lBody (a :& RNil)
  pure $ res <+> r
runInstrImpl runner (DIP i) (a :& r) = do
  consume Gas.stackOp
  res <- runner i r
  pure $ a :& res
runInstrImpl _ FAILWITH (a :& _) = throwError $ MichelsonFailedWith a
runInstrImpl _ CAST (a :& r) = pure $ a :& r
runInstrImpl _ RENAME (a :& r) = pure $ a :& r
runInstrImpl _ PACK (a :& r) = pure $ (VC $ CvBytes $ packValue' a) :& r
runInstrImpl _ UNPACK (VC (CvBytes a) :& r) = do
  env <- asks ceContracts
  v <- case runUnpack env a of
    Left (_, cost) -> do consume cost; pure Nothing
    Right v -> pure $ Just v
  pure $ VOption v :& r
runInstrImpl _ CONCAT (a :& b :& r) =
  do consume $ costConcat a b; pure $ evalConcat a b :& r
runInstrImpl _ CONCAT' (VList a :& r) =
  do consume $ costConcat' a; pure $ evalConcat' a :& r
runInstrImpl _ SLICE (VC (CvNat o) :& VC (CvNat l) :& s :& r) =
  do consume $ costSlice o l s; pure $ VOption (evalSlice o l s) :& r
runInstrImpl _ ISNAT (VC (CvInt i) :& r) = do
  consume $ Gas.abs i
  if i < 0
  then pure $ VOption Nothing :& r
  else pure $ VOption (Just $ VC (CvNat $ fromInteger i)) :& r
runInstrImpl _ ADD (VC l :& VC r :& rest) =
  (:& rest) <$> runArithOp (Proxy @Add) l r
runInstrImpl _ SUB (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Sub) l r
runInstrImpl _ MUL (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Mul) l r
runInstrImpl _ EDIV (VC l :& VC r :& rest) = pure $ evalEDivOp l r :& rest
runInstrImpl _ ABS (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Abs) a) :& rest
runInstrImpl _ NEG (VC a :& rest) = pure $ VC (evalUnaryArithOp (Proxy @Neg) a) :& rest
runInstrImpl _ LSL (VC x :& VC s :& rest) = (:& rest) <$> runArithOp (Proxy @Lsl) x s
runInstrImpl _ LSR (VC x :& VC s :& rest) = (:& rest) <$> runArithOp (Proxy @Lsr) x s
runInstrImpl _ OR (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Or) l r
runInstrImpl _ AND (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @And) l r
runInstrImpl _ XOR (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Xor) l r
runInstrImpl _ NOT (VC a :& rest) = (:& rest) <$> runUnaryArithOp (Proxy @Not) a
runInstrImpl _ COMPARE (VC l :& VC r :& rest) = (:& rest) <$> runArithOp (Proxy @Compare) l r
runInstrImpl _ EQ (VC a :& rest) = (:& rest) <$> runUnaryArithOp (Proxy @Eq') a
runInstrImpl _ NEQ (VC a :& rest) = (:& rest) <$> runUnaryArithOp (Proxy @Neq) a
runInstrImpl _ LT (VC a :& rest) = (:& rest) <$> runUnaryArithOp (Proxy @Lt) a
runInstrImpl _ GT (VC a :& rest) = (:& rest) <$> runUnaryArithOp (Proxy @Gt) a
runInstrImpl _ LE (VC a :& rest) = (:& rest) <$> runUnaryArithOp (Proxy @Le) a
runInstrImpl _ GE (VC a :& rest) = (:& rest) <$> runUnaryArithOp (Proxy @Ge) a
runInstrImpl _ INT (VC (CvNat n) :& r) =
  do consume Gas.int; pure $ VC (CvInt $ toInteger n) :& r
runInstrImpl _ SELF r = do
  consume Gas.self
  ContractEnv{..} <- ask
  pure $ VContract ceSelf :& r
runInstrImpl _ (CONTRACT (nt :: T.Notes p)) (VC (CvAddress addr) :& r) = do
  consume Gas.contract
  ContractEnv{..} <- ask
  case Map.lookup addr ceContracts of
    Just tc -> do
      pure $
        either (const $ VOption Nothing)
               (const $ VOption (Just $ VContract addr))
               (compareTypes (sing @p, nt) tc)
        :& r
    Nothing -> pure $ VOption Nothing :& r
runInstrImpl _ TRANSFER_TOKENS (p :& VC (CvMutez mutez) :& contract :& r) = do
  consume Gas.transfer
  pure $ VOp (OpTransferTokens $ TransferTokens p mutez contract) :& r
runInstrImpl _ SET_DELEGATE (VOption mbKeyHash :& r) = do
  consume Gas.setDelegate
  case mbKeyHash of
    Just (VC (CvKeyHash k)) -> pure $ VOp (OpSetDelegate $ SetDelegate $ Just k) :& r
    Nothing -> pure $ VOp (OpSetDelegate $ SetDelegate $ Nothing) :& r
runInstrImpl _ CREATE_ACCOUNT
  (VC (CvKeyHash k) :& VOption mbKeyHash :&
    (VC (CvBool spendable)) :& (VC (CvMutez m)) :& r) = do
  consume Gas.createAccount
  pure (VOp (OpCreateAccount $ CreateAccount k (unwrapMbKeyHash mbKeyHash) spendable m)
    :& (VC . CvAddress) (KeyAddress k) :& r)
runInstrImpl _ (CREATE_CONTRACT ops)
  (VC (CvKeyHash k) :& VOption mbKeyHash :& (VC (CvBool spendable)) :&
    (VC (CvBool delegetable)) :& (VC (CvMutez m)) :& g :& r) = do
  consume Gas.createContract
  pure (VOp (OpCreateContract $
    CreateContract k (unwrapMbKeyHash mbKeyHash) spendable delegetable m g ops)
    :& (VC . CvAddress) (U.mkContractAddress $
      createOrigOp k mbKeyHash spendable delegetable m ops g) :& r)
runInstrImpl _ IMPLICIT_ACCOUNT (VC (CvKeyHash k) :& r) = do
  consume Gas.implicitAccount
  pure $ VContract (KeyAddress k) :& r
runInstrImpl _ NOW r = do
  consume Gas.now
  ContractEnv{..} <- ask
  pure $ VC (CvTimestamp ceNow) :& r
runInstrImpl _ AMOUNT r = do
  consume Gas.amount
  ContractEnv{..} <- ask
  pure $ VC (CvMutez ceAmount) :& r
runInstrImpl _ BALANCE r = do
  consume Gas.balance
  ContractEnv{..} <- ask
  pure $ VC (CvMutez ceBalance) :& r
runInstrImpl _ CHECK_SIGNATURE (VKey k :& VSignature v :&
  VC (CvBytes b) :& r) =
  do consume Gas.checkSignature; pure $ VC (CvBool $ checkSignature k v b) :& r
runInstrImpl _ SHA256 (VC (CvBytes b) :& r) = do
  consume $ Gas.hash b 32
  pure $ VC (CvBytes $ sha256 b) :& r
runInstrImpl _ SHA512 (VC (CvBytes b) :& r) = do
  consume $ Gas.hash b 64
  pure $ VC (CvBytes $ sha512 b) :& r
runInstrImpl _ BLAKE2B (VC (CvBytes b) :& r) = do
  consume $ Gas.hash b 32
  pure $ VC (CvBytes $ blake2b b) :& r
runInstrImpl _ HASH_KEY (VKey k :& r) =
  do consume Gas.hashKey; pure $ VC (CvKeyHash $ hashKey k) :& r
runInstrImpl _ STEPS_TO_QUOTA r = do
  consume Gas.stepsToQuota
  RemainingSteps x <- gets isRemainingSteps
  pure $ VC (CvNat $ (fromInteger . toInteger) x) :& r
runInstrImpl _ SOURCE r = do
  consume Gas.source
  ContractEnv{..} <- ask
  pure $ VC (CvAddress ceSource) :& r
runInstrImpl _ SENDER r = do
  consume Gas.source
  ContractEnv{..} <- ask
  pure $ VC (CvAddress ceSender) :& r
runInstrImpl _ ADDRESS (VContract a :& r) =
  do consume Gas.address; pure $ VC (CvAddress a) :& r

-- | Evaluates an arithmetic operation and either fails or proceeds.
runArithOp
  :: (ArithOp aop n m, Typeable n, Typeable m)
  => proxy aop
  -> CValue n
  -> CValue m
  -> EvalOp (T.Value' instr ('Tc (ArithRes aop n m)))
runArithOp op l r = do
  consume $ costArithOp op l r
  case evalOp op l r of
    Left  err -> throwError (MichelsonArithError err)
    Right res -> pure (T.VC res)

runUnaryArithOp
  :: UnaryArithOp aop n
  => proxy aop
  -> CValue n
  -> EvalOp (T.Value' instr ('Tc (UnaryArithRes aop n)))
runUnaryArithOp op n = do
  consume $ costUnaryOp op n
  pure (T.VC $ evalUnaryArithOp op n)

-- | Unpacks given raw data into a typed value.
runUnpack
  :: forall t. (SingI t, HasNoOp t, HasNoBigMap t)
  => TcOriginatedContracts
  -> ByteString
  -> Either (UnpackError, Cost) (T.Value t)
runUnpack contracts bs =
  -- TODO [TM-80] Gas consumption here should depend on unpacked data size
  -- and size of resulting expression, errors would also spend some (all equally).
  -- Fortunatelly, the inner decoding logic does not need to know anything about gas use.
  case unpackValue' (UnpackEnv contracts) bs of
    Left e -> Left (e, Gas.unpackFailed bs)
    Right v -> Right v

createOrigOp
  :: (SingI param, SingI store, HasNoOp store)
  => KeyHash
  -> Maybe (T.Value ('Tc 'U.CKeyHash))
  -> Bool -> Bool -> Mutez
  -> Contract param store
  -> Value' Instr store
  -> U.OriginationOperation
createOrigOp k mbKeyHash delegetable spendable m contract g =
  U.OriginationOperation
    { ooManager = k
    , ooDelegate = (unwrapMbKeyHash mbKeyHash)
    , ooSpendable = spendable
    , ooDelegatable = delegetable
    , ooBalance = m
    , ooStorage = untypeValue g
    , ooContract = convertContract contract
    }

unwrapMbKeyHash :: Maybe (T.Value ('Tc 'U.CKeyHash)) -> Maybe KeyHash
unwrapMbKeyHash (Just (T.VC (CvKeyHash keyHash))) = Just keyHash
unwrapMbKeyHash Nothing = Nothing

interpretExt :: SomeItStack -> EvalOp ()
interpretExt (SomeItStack (T.PRINT (T.PrintComment pc)) st) = do
  let getEl (Left l) = l
      getEl (Right str) = withStackElem str st show
  modify (\s -> s {isMorleyLogs = MorleyLogs $ mconcat (map getEl pc) : unMorleyLogs (isMorleyLogs s)})

interpretExt (SomeItStack (T.TEST_ASSERT (T.TestAssert nm pc instr)) st) = do
  ost <- runInstrNoGas instr st
  let ((T.fromVal -> succeeded) :& _) = ost
  unless succeeded $ do
    interpretExt (SomeItStack (T.PRINT pc) st)
    throwError $ MichelsonFailedTestAssert $ "TEST_ASSERT " <> nm <> " failed"

-- | Access given stack reference (in CPS style).
withStackElem
  :: forall st a.
     T.StackRef st
  -> Rec T.Value st
  -> (forall t. T.Value t -> a)
  -> a
withStackElem (T.StackRef sn) vals cont =
  loop (vals, sn)
  where
    loop
      :: forall s (n :: Peano). (LongerThan s n)
      => (Rec T.Value s, Sing n) -> a
    loop = \case
      (e :& _, SZ) -> cont e
      (_ :& es, SS n) -> loop (es, n)
