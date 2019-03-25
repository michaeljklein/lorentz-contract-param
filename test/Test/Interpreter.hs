module Test.Interpreter
  ( spec
  ) where

import Fmt (pretty)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, label, (.&&.), (===))

import Michelson.Interpret (ContractEnv(..), ContractReturn, MichelsonFailed(..), RemainingSteps)
import Michelson.Typed (CT(..), CValue(..), Instr(..), T(..), toVal, ( # ))
import qualified Michelson.Typed as T
import Morley.Ext (interpretMorley)
import Morley.Test (ContractPropValidator, contractProp, specWithTypedContract)
import Morley.Types (MorleyLogs)
import Test.Interpreter.Auction (auctionSpec)
import Test.Interpreter.CallSelf (selfCallerSpec)
import Test.Interpreter.Compare (compareSpec)
import Test.Interpreter.Conditionals (conditionalsSpec)
import Test.Interpreter.StringCaller (stringCallerSpec)
import Test.Util.Interpreter (dummyContractEnv)

spec :: Spec
spec = describe "Advanced type interpreter tests" $ do
  let contractResShouldBe (res, _) expected =
        case res of
          Left err -> expectationFailure $ "Unexpected failure: " <> pretty err
          Right (_ops, v) -> v `shouldBe` expected

  specWithTypedContract "contracts/basic5.tz" $ \contract ->
    it "Basic test" $
      interpretMorley contract T.VUnit (toVal [1 :: Integer]) dummyContractEnv
        `contractResShouldBe` (toVal [13 :: Integer, 100])

  specWithTypedContract "contracts/increment.tz" $ \contract ->
    it "Basic test" $
      interpretMorley contract T.VUnit (toVal @Integer 23) dummyContractEnv
        `contractResShouldBe` (toVal @Integer 24)

  specWithTypedContract "contracts/fail.tz" $ \contract ->
    it "Fail test" $
      interpretMorley contract T.VUnit T.VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)

  specWithTypedContract "contracts/mutez_add_overflow.tz" $ \contract ->
    it "Mutez add overflow test" $
      interpretMorley contract T.VUnit T.VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)

  specWithTypedContract "contracts/mutez_sub_underflow.tz" $ \contract ->
    it "Mutez sub underflow test" $
      interpretMorley contract T.VUnit T.VUnit dummyContractEnv
        `shouldSatisfy` (isLeft . fst)

  specWithTypedContract "contracts/basic1.tz" $ \contract -> do
    prop "Random check" $
      contractProp contract validateBasic1 dummyContractEnv

  auctionSpec
  compareSpec
  conditionalsSpec
  stringCallerSpec
  selfCallerSpec

  specWithTypedContract "contracts/steps_to_quota_test1.tz" $ \contract -> do
    it "Amount of steps should reduce" $ do
      validateStepsToQuotaTest
        (interpretMorley contract T.VUnit (T.VC (CvNat 0)) dummyContractEnv) 4

  specWithTypedContract "contracts/steps_to_quota_test2.tz" $ \contract -> do
    it "Amount of steps should reduce" $ do
      validateStepsToQuotaTest
        (interpretMorley contract T.VUnit (T.VC (CvNat 0)) dummyContractEnv) 8

  specWithTypedContract "contracts/gas_exhaustion.tz" $ \contract -> do
    it "Contract should fail due to gas exhaustion" $ do
      case fst $ interpretMorley contract (T.VC (CvString "x")) (T.VC (CvString "x")) dummyContractEnv of
        Right _ -> expectationFailure "expecting contract to fail"
        Left MichelsonGasExhaustion -> pass
        Left _ -> expectationFailure "expecting another failure reason"

validateBasic1
  :: ContractPropValidator 'TUnit ('TList ('Tc 'CInt)) Property
validateBasic1 _env _param input (Right (ops, res), _) =
    (trToList res === [calcSum input + 12, 100])
    .&&.
    (label "returned no ops" $ null ops)
  where
    calcSum :: T.Value' instr ('TList ('Tc 'CInt)) -> Integer
    calcSum (T.VList l) = sum $ map (\(T.VC (CvInt i)) -> i) l

    trToList :: T.Value' instr ('TList ('Tc 'CInt)) -> [Integer]
    trToList (T.VList l) = map (\(T.VC (CvInt i)) -> i) l

validateBasic1 _ _ _ (Left e, _) = error $ show e

validateStepsToQuotaTest ::
     ContractReturn MorleyLogs ('Tc 'CNat) -> RemainingSteps -> Expectation
validateStepsToQuotaTest res numOfSteps =
  case fst res of
    Right ([], T.VC (CvNat x)) ->
      (fromInteger . toInteger) x `shouldBe` ceMaxSteps dummyContractEnv - numOfSteps
    _ -> expectationFailure "unexpected contract result"

--------------------
-- Examples
--------------------

-- | @myInstr@ is an equivalent to Michelson code:
--
--    PUSH int 223;
--    SOME;
--    IF_NONE { DUP; } { SWAP; };
--    ADD;
--    PUSH nat 12
--    ADD;
_myInstr :: Typeable s => Instr ('Tc 'CInt : s) ('Tc 'CInt : s)
_myInstr =
  PUSH (T.VC $ CvInt 223) #
  SOME #
  IF_NONE DUP SWAP #
  ADD #
  PUSH (T.VC $ CvNat 12) #
  ADD

_myInstr2 :: Typeable a => Instr a ('TOption ('Tc 'CInt) : a)
_myInstr2 =
  PUSH (T.VOption $ Just $ T.VC $ CvInt 223) #
  Nop
