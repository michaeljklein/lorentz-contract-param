-- | Tests for Morley.Runtime.

module Test.Morley.Runtime
  ( spec
  ) where

import Control.Lens (at)
import Fmt (pretty)
import Test.Hspec
  (Expectation, Spec, context, describe, expectationFailure, it, parallel, runIO, shouldBe,
  shouldSatisfy, specify)

import Michelson.Interpret (ContractEnv(..), InterpretUntypedError(..), InterpretUntypedResult(..))
import Michelson.Typed (unsafeValToValue)
import qualified Michelson.Untyped as U
import Morley.Ext (interpretMorleyUntyped)
import Morley.Runtime
import Morley.Runtime.GState (GState(..), initGState)
import Morley.Types (MorleyLogs)
import Test.Util.Interpreter (ContractAux(..), dummyContractEnv, dummyMaxSteps, dummyNow)
import Tezos.Address (Address(..))
import Tezos.Core (unsafeMkMutez)

import Test.Util.Interpreter (dummyOrigination)

spec :: Spec
spec = describe "Morley.Runtime" $ do
  illTypedContract <- runIO $
    prepareContract (Just "contracts/ill-typed/sum-strings.tz")

  -- Safe to run in parallel, because 'interpreterPure' is pure.
  describe "interpreterPure" $ parallel $ do
    context "Updates storage value of executed contract" $ do
      specify "contract1" $ updatesStorageValue contractAux1
      specify "contract2" $ updatesStorageValue contractAux2
    it "Fails to originate an already originated contract" failsToOriginateTwice
    it "Fails to originate an ill-typed contract"
      (failsToOriginateIllTyped (U.ValueString "") illTypedContract)

----------------------------------------------------------------------------
-- Test code
----------------------------------------------------------------------------

data UnexpectedFailed =
  UnexpectedFailed (InterpretUntypedError MorleyLogs)
  deriving (Show)

instance Exception UnexpectedFailed

updatesStorageValue :: ContractAux -> Expectation
updatesStorageValue ca = either throwM handleResult $ do
  let
    contract = caContract ca
    ce = caEnv ca
    origination = (dummyOrigination (caStorage ca) contract)
      { U.ooBalance = ceBalance ce
      }
    addr = U.mkContractAddress origination
    txData = TxData
      { tdSenderAddress = ceSender ce
      , tdParameter = caParameter ca
      , tdAmount = unsafeMkMutez 100
      }
    interpreterOps =
      [ OriginateOp origination
      , TransferOp addr txData
      ]
  (addr,) <$> interpreterPure dummyNow dummyMaxSteps initGState interpreterOps
  where
    toNewStorage :: InterpretUntypedResult MorleyLogs -> U.Value
    toNewStorage InterpretUntypedResult {..} = unsafeValToValue iurNewStorage

    handleResult :: (Address, InterpreterRes) -> Expectation
    handleResult (addr, ir) = do
      expectedValue <-
        either (throwM . UnexpectedFailed) (pure . toNewStorage) $
        interpretMorleyUntyped
                  (caContract ca) (caParameter ca) (caStorage ca) (caEnv ca)
      case gsAddresses (_irGState ir) ^. at addr of
        Nothing -> expectationFailure $ "Address not found: " <> pretty addr
        Just (ASContract cs) -> csStorage cs `shouldBe` expectedValue
        Just _ -> expectationFailure $ "Address has unexpected state " <> pretty addr

failsToOriginateTwice :: Expectation
failsToOriginateTwice =
  simpleTest ops isAlreadyOriginated
  where
    contract = caContract contractAux1
    origination = dummyOrigination (caStorage contractAux1) contract
    ops = [OriginateOp origination, OriginateOp origination]
    isAlreadyOriginated (Left (IEAlreadyOriginated {})) = True
    isAlreadyOriginated _ = False

failsToOriginateIllTyped :: U.Value -> U.Contract -> Expectation
failsToOriginateIllTyped initialStorage illTypedContract =
  simpleTest ops isIllTypedContract
  where
    origination = dummyOrigination initialStorage illTypedContract
    ops = [OriginateOp origination]
    isIllTypedContract (Left (IEIllTypedContract {})) = True
    isIllTypedContract _ = False

simpleTest ::
     [InterpreterOp]
  -> (Either InterpreterError InterpreterRes -> Bool)
  -> Expectation
simpleTest ops predicate =
  interpreterPure dummyNow dummyMaxSteps initGState ops `shouldSatisfy`
  predicate

----------------------------------------------------------------------------
-- Data
----------------------------------------------------------------------------

contractAux1 :: ContractAux
contractAux1 = ContractAux
  { caContract = contract
  , caEnv = dummyContractEnv
  , caStorage = U.ValueTrue
  , caParameter = U.ValueString "aaa"
  }
  where
    contract :: U.Contract
    contract = U.Contract
      { para = U.Type U.tstring U.noAnn
      , stor = U.Type U.tbool U.noAnn
      , code =
        [ U.Op $ U.CDR U.noAnn U.noAnn
        , U.Op $ U.NIL U.noAnn U.noAnn $ U.Type U.TOperation U.noAnn
        , U.Op $ U.PAIR U.noAnn U.noAnn U.noAnn U.noAnn
        ]
      }

contractAux2 :: ContractAux
contractAux2 = contractAux1
  { caContract = (caContract contractAux1)
    { U.code =
      [ U.Op $ U.CDR U.noAnn U.noAnn
      , U.Op $ U.NOT U.noAnn
      , U.Op $ U.NIL U.noAnn U.noAnn $ U.Type U.TOperation U.noAnn
      , U.Op $ U.PAIR U.noAnn U.noAnn U.noAnn U.noAnn
      ]
    }
  }
