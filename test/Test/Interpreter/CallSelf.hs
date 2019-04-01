-- | Tests for the contract that calls self several times.

module Test.Interpreter.CallSelf
  ( selfCallerSpec
  ) where

import Test.Hspec (Expectation, Spec, it, parallel, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Gen, choose, forAll)

import Michelson.Interpret (ContractEnv(..), InterpreterState(..), RemainingSteps(..))
import Michelson.Typed
import Michelson.Untyped (OriginationOperation(..), UntypedContract, mkContractAddress)
import qualified Michelson.Untyped as Untyped
import Morley.Runtime (InterpreterOp(..), TxData(..))
import Morley.Runtime.GState
import Morley.Test (ContractPropValidator, contractProp, specWithContract)
import Morley.Test.Dummy
import Morley.Test.Integrational

selfCallerSpec :: Spec
selfCallerSpec =
  parallel $
  specWithContract "contracts/call_self_several_times.tz" $ \selfCaller ->
  specImpl selfCaller

data Fixture = Fixture
  { fMaxSteps :: RemainingSteps
  , fParameter :: !Word64
  } deriving Show

gasForOneExecution :: Num a => a
gasForOneExecution = 19

gasForLastExecution :: Num a => a
gasForLastExecution = 20

fExpectSuccess :: Fixture -> Bool
fExpectSuccess Fixture {..} =
  -- note: fParameter must be ≥ 1
  fMaxSteps >= fromIntegral ((fParameter - 1) * gasForOneExecution + gasForLastExecution)

genFixture :: Gen Fixture
genFixture =
  Fixture <$> (RemainingSteps <$> choose (minGas, maxGas)) <*> choose (minCalls, maxCalls)
  where
    minCalls = 1
    maxCalls = 10
    minGas = 0
    maxGas = gasForOneExecution * maxCalls

type Parameter = 'Tc 'CInt
type Storage = 'Tc 'CNat

specImpl ::
     (UntypedContract, Contract Parameter Storage)
  -> Spec
specImpl (uSelfCaller, selfCaller) = modifyMaxSuccess (min 10) $ do
  it ("With parameter 1 single execution consumes " <>
      show @_ @Int gasForLastExecution <> " gas") $
    contractProp selfCaller (unitValidator gasForLastExecution) unitContractEnv
    (1 :: Integer) (0 :: Natural)

  it ("With parameter 2 single execution consumes " <>
      show @_ @Int gasForOneExecution <> " gas") $
    contractProp selfCaller (unitValidator gasForOneExecution) unitContractEnv
    (2 :: Integer) (0 :: Natural)

  prop propertyDescription $
    forAll genFixture $ \fixture ->
      integrationalTestProperty dummyNow (fMaxSteps fixture)
      (operations fixture) (integValidator fixture)
  where
    -- Environment for unit test
    unitContractEnv = dummyContractEnv
    -- Validator for unit test
    unitValidator ::
      RemainingSteps -> ContractPropValidator Storage Expectation
    unitValidator gasDiff (_, isRemainingSteps -> remSteps) =
      remSteps `shouldBe` ceMaxSteps unitContractEnv - gasDiff

    propertyDescription =
      "calls itself as many times as you pass to it as a parameter, " <>
      "it fails due to gas limit if the number is large, otherwise the " <>
      "storage is updated to the number of calls"

    operations :: Fixture -> [InterpreterOp]
    operations fixture = [originateOp, transferOp fixture]

    origination :: OriginationOperation
    origination = dummyOrigination (Untyped.ValueInt 0) uSelfCaller
    address = mkContractAddress origination
    originateOp = OriginateOp origination

    txData :: Fixture -> TxData
    txData fixture = TxData
      { tdSenderAddress = genesisAddress
      , tdParameter = Untyped.ValueInt (fromIntegral $ fParameter fixture)
      , tdAmount = minBound
      }
    transferOp fixture = TransferOp address (txData fixture)

    integValidator :: Fixture -> IntegrationalValidator
    integValidator fixture
      | fExpectSuccess fixture =
        let expectedStorage =
              Untyped.ValueInt (fromIntegral $ fParameter fixture)
         in Right $ expectStorageConstant address expectedStorage
      | otherwise = Left expectGasExhaustion
