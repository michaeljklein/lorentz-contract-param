module Test.Ext
  ( spec_Ext_Intepreter
  , test_STACKTYPE
  ) where

import Data.Default (def)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.HUnit (Assertion, assertFailure)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Michelson.Interpret (InterpreterState(..), MorleyLogs(..), interpret)
import Michelson.Test (specWithTypedContract)
import Michelson.Test.Dummy (dummyContractEnv)
import Michelson.TypeCheck (HST(..), SomeHST(..), runTypeCheck, typeCheckExt, typeCheckList)
import Michelson.Typed (CValue(..), extractNotes, fromUType, withSomeSingT)
import qualified Michelson.Typed as T
import Michelson.Untyped
  (CT(..), ExpandedExtInstr, ExtInstrAbstract(..), StackTypePattern(..), T(..), TyVar(..),
  Type(..), ann, noAnn)

spec_Ext_Intepreter :: Spec
spec_Ext_Intepreter = describe "PRINT/TEST_ASSERT tests" $ do
  specWithTypedContract "contracts/testassert_square.mtz" $
    testAssertSquareSpec
  specWithTypedContract "contracts/testassert_square2.mtz" $
    testAssertSquareSpec
  where
    testAssertSquareSpec c = do
      it "TEST_ASSERT assertion passed" $ do
        runTest True c 100 100
        runTest True c 1 1
      it "TEST_ASSERT assertion failed" $ do
        runTest False c 0 100
        runTest False c -1 -2

    runTest corr contract x y = do
      let x' = T.VC $ T.CvInt x :: T.Value ('T.Tc 'T.CInt)
      let y' = T.VC $ T.CvInt y :: T.Value ('T.Tc 'T.CInt)
      let area' = T.VC $ CvInt $ x * y :: T.Value ('T.Tc 'T.CInt)
      let check (a, InterpreterState s _ _) =
            if corr then isRight a && s == MorleyLogs ["Area is " <> show area']
            else isLeft a && s == MorleyLogs ["Sides are " <> show x' <> " x " <> show y']
      interpret contract (T.VPair (x', y')) T.VUnit dummyContractEnv `shouldSatisfy` check

test_STACKTYPE :: [TestTree]
test_STACKTYPE =
  [ testCase "Correct test on [] pattern" $ runExtTest test1 True
  , testCase "Correct test on [a, b] pattern" $ runExtTest test2 True
  , testCase "Correct test on [a, b, ...] pattern" $ runExtTest test3 True
  , testCase "Correct test on [a, b, ...] pattern and stack [a, b]" $ runExtTest test4 True

  , testCase "Failed test on [] pattern and stack [a]" $ runExtTest test5 False
  , testCase "Failed test on [a, b] pattern and stack [a, b, c]" $ runExtTest test6 False
  , testCase "Failed test on [a, b] pattern and stack [a]" $ runExtTest test7 False
  , testCase "Failed test on [a, b, ...] pattern and stack [a]" $ runExtTest test8 False
  , testCase "Failed test on [a, b] pattern and stack [a, c]" $ runExtTest test9 False
  ]
  where
    p2 = StkCons (TyCon t1) (StkCons (TyCon t2) StkEmpty)
    p3 = StkCons (TyCon t1) (StkCons (TyCon t2) StkRest)

    test1 = (STACKTYPE StkEmpty, convertToHST [])
    test2 = (STACKTYPE p2, convertToHST [t1, t2])
    test3 = (STACKTYPE p3, convertToHST [t1, t2, t3])
    test4 = (STACKTYPE p3, convertToHST [t1, t2])

    test5 = (STACKTYPE StkEmpty, convertToHST [t1])
    test6 = (STACKTYPE p2, convertToHST [t1, t2, t3])
    test7 = (STACKTYPE p2, convertToHST [t1])
    test8 = (STACKTYPE p3, convertToHST [t1])
    test9 = (STACKTYPE p2, convertToHST [t1, t3])

    t1 = Type (TOption (ann "f") (Type TKey (ann "key"))) (ann "opt")
    t2 = Type (TPair (ann "f") (ann "s") (Type TUnit "x") (Type TSignature "s")) noAnn
    t3 = Type (Tc CInt) (ann "tint")

    convertToHST :: [Type] -> SomeHST
    convertToHST [] = SomeHST SNil
    convertToHST (t : ts) = withSomeSingT (fromUType t) $ \sing ->
      let nt = fromRight (error "unexpected trouble with extracting annotations")
                         (extractNotes t sing) in
      case convertToHST ts of
        SomeHST is -> SomeHST ((sing, nt, noAnn) ::& is)

    nh (ni, si) =
      runTypeCheck (Type TKey noAnn) mempty $ usingReaderT def $ typeCheckExt typeCheckList ni si

    runExtTest :: (ExpandedExtInstr, SomeHST) -> Bool -> Assertion
    runExtTest (ui, SomeHST hst) correct = case (nh (ui, hst), correct) of
      (Right _, False) -> assertFailure $ "Test expected to fail but it passed"
      (Left e, True)   -> assertFailure $ "Test expected to pass but it failed with error: " <> show e
      _                -> pass
