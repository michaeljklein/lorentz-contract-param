module Michelson.TypeCheck.TypeCheck
  ( TcInstrHandler
  , TcOriginatedContracts
  , TcResult
  , TypeCheckEnv (..)
  , TypeCheck
  , runTypeCheck
  , runTypeCheckCValue
  , runTypeCheckReturnState
  , TypeCheckInstr
  , TypeCheckCValue
  , runTypeCheckTest

  , tcContractParamL
  , tcContractsL
  , tcExtFramesL

  , consume
  ) where

import Control.Lens (makeLensesWith, (<>=))

import Gas.Type (Cost)
import Michelson.ErrorPos (InstrCallStack)
import Michelson.TypeCheck.Error (TCError, TCTypeError)
import Michelson.TypeCheck.Types
import qualified Michelson.Untyped as U
import Tezos.Address (Address)
import Util.Lens

type TypeCheck a =
  ExceptT TCError
    (State TypeCheckEnv) a

type TypeCheckCValue op a =
  ExceptT (U.Value' op, TCTypeError)
    (State TypeCheckEnv) a

runTypeCheckCValue
  :: TypeCheckEnv -> TypeCheckCValue op a
  -> (Either (U.Value' op, TCTypeError) a, TypeCheckEnv)
runTypeCheckCValue env act = runState (runExceptT act) env

type TcOriginatedContracts = Map Address U.Type

-- | The typechecking state
data TypeCheckEnv = TypeCheckEnv
  { tcExtFrames     :: TcExtFrames
  , tcContractParam :: U.Type
  , tcContracts     :: TcOriginatedContracts
  , tcCost          :: Cost
  }

makeLensesWith postfixLFields ''TypeCheckEnv

consume :: (MonadState TypeCheckEnv m) => Cost -> m ()
-- consume cost = do
--   oldEnv <- get
--   modify (\s -> s {tcCost = (tcCost oldEnv) <> cost})
consume cost = tcCostL <>= cost

runTypeCheck :: U.Type -> TcOriginatedContracts -> TypeCheck a -> Either TCError a
runTypeCheck param contracts act =
  evaluatingState (TypeCheckEnv [] param contracts mempty) $ runExceptT act

runTypeCheckReturnState ::
  U.Type -> TcOriginatedContracts -> TypeCheck a -> (Either TCError a, TypeCheckEnv)
runTypeCheckReturnState param contracts act =
  runState (runExceptT act) (TypeCheckEnv [] param contracts mempty)

-- | Run type checker as if it worked isolated from other world -
-- no access to environment of the current contract is allowed.
--
-- Use this function for test purposes only.
runTypeCheckTest :: TypeCheck a -> Either TCError a
runTypeCheckTest = evaluatingState initSt . runExceptT
  where
  initSt =
    TypeCheckEnv
    { tcExtFrames = []
    , tcContractParam = error "Contract param touched"
    , tcContracts = mempty
    , tcCost = mempty
    }

type TcResult inp = Either TCError (SomeInstr inp)

type TypeCheckInstr a =
       ReaderT InstrCallStack (ExceptT TCError (State TypeCheckEnv)) a

-- pva701: it's really painful to add arguments to TcInstrHandler
-- due to necessity to refactor @typeCheckInstr@.
-- Also functions which are being called from @typeCheckInstr@ would
-- have to be refactored too.
-- Therefore, I am using ReaderT over TypeCheck.
type TcInstrHandler
   = forall inp. Typeable inp
      => U.ExpandedInstr
      -> HST inp
      -> TypeCheckInstr (SomeInstr inp)
