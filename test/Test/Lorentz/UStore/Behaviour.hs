{-# LANGUAGE DerivingStrategies #-}

-- | Tests for Lorentz 'UStore'.
module Test.Lorentz.UStore.Behaviour
  ( test_Roundtrip
  , test_Conversions
  ) where

import Data.Default (def)
import qualified Data.Map as M
import Data.Vinyl.Core (Rec(..))
import Test.HUnit (Assertion, assertFailure, (@?=))
import Test.QuickCheck (Arbitrary(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz.Base
import Lorentz.Instr as L
import Lorentz.UStore
import Michelson.Interpret.Unpack
import Michelson.Test.Dummy
import Michelson.Test.Util
import Util.Test.Arbitrary ()

import Test.Util.QuickCheck (roundtripTest)


data MyTemplate f = MyTemplate
  { ints :: f -/ Integer |~> ()
  , bool :: f -/ UStoreField Bool
  } deriving stock (Generic)

deriving stock instance Eq (MyTemplate UStoreValue)
deriving stock instance Show (MyTemplate UStoreValue)

instance Arbitrary (MyTemplate UStoreValue) where
  arbitrary = MyTemplate <$> arbitrary <*> arbitrary

data MyTemplateBig f = MyTemplateBig
  { small :: MyTemplate f
  , bytes :: f -/ ByteString |~> Natural
  , total :: f -/ UStoreField Integer
  } deriving stock (Generic)

deriving stock instance Eq (MyTemplateBig UStoreValue)
deriving stock instance Show (MyTemplateBig UStoreValue)

instance Arbitrary (MyTemplateBig UStoreValue) where
  arbitrary = MyTemplateBig <$> arbitrary <*> arbitrary <*> arbitrary

test_Roundtrip :: [TestTree]
test_Roundtrip =
  [ roundtripTest (mkUStore @MyTemplate) (ustoreDecomposeFull dummyUnpackEnv)
  , roundtripTest (mkUStore @MyTemplateBig) (ustoreDecomposeFull dummyUnpackEnv)
  ]

test_Conversions :: [TestTree]
test_Conversions =
  [ testGroup "Simple store template"
    [ testCase "No action" $
        ustoreChangeTest
          ( nop
          , MyTemplate def False
          , MyTemplate def False
          )
    , testCase "Insert into submap" $
        ustoreChangeTest
          ( unit # push 5 # ustoreInsert #ints
          , MyTemplate def False
          , MyTemplate (one (5, ())) False
          )
    , testCase "Delete from submap" $
        ustoreChangeTest
          ( push 3 # ustoreDelete #ints
          , MyTemplate (one (3, ())) False
          , MyTemplate mempty False
          )
    , testCase "Get from submap" $
        ustoreChangeTest
          ( dup # push 0 # ustoreGet #ints #
            ifNone (push 10) (L.drop # push 11) # dip unit # ustoreInsert #ints
          , MyTemplate (one (0, ())) False
          , MyTemplate (M.fromList [(0, ()), (11, ())]) False
          )
    , testCase "Set field" $
        ustoreChangeTest
          ( push True # ustoreSetField #bool
          , MyTemplate mempty False
          , MyTemplate mempty True
          )
    , testCase "Get field" $
        ustoreChangeTest
          ( ustoreGetField #bool #
            if_ (push 5) (push 0) # dip unit # ustoreInsert #ints
          , MyTemplate mempty False
          , MyTemplate (one (0, ())) False
          )
    , testCase "Leave some entries untouched" $
        ustoreChangeTest
          ( push 0 # ustoreDelete #ints #
            unit # push 2 # ustoreInsert #ints
          , MyTemplate (M.fromList [(0, ()), (1, ())]) False
          , MyTemplate (M.fromList [(1, ()), (2, ())]) False
          )
    ]

  , testGroup "Non-flat store template"
    [ testCase "Custom scenario 1" $
        ustoreChangeTest
          ( push "a" # ustoreDelete #bytes #
            push 2 # push "b" # ustoreInsert #bytes #
            ustoreGetField #total # push @Integer 1 # add # ustoreSetField #total #
            unit # push 0 # ustoreInsert #ints #
            push True # ustoreSetField #bool
          , MyTemplateBig
            { small = MyTemplate def False
            , bytes = one ("a", 1)
            , total = 10
            }
          , MyTemplateBig
            { small = MyTemplate (one (0, ())) True
            , bytes = one ("b", 2)
            , total = 11
            }
          )
    ]
  ]
  where
    -- We accept a tuple as argument to avoid many parentheses
    ustoreChangeTest
      :: ( Each [Eq, Show, Generic] '[template UStoreValue]
         , Generic (template UStoreTypesOnly)
         , UStoreConversible template
         , HasCallStack
         )
      => ( '[UStore template] :-> '[UStore template]
         , template UStoreValue
         , template UStoreValue
         )
      -> Assertion
    ustoreChangeTest (instr, initStoreHs, expectedNewStore) =
      let
        initStore = mkUStore initStoreHs
        (Identity ustore :& RNil) =
          leftToPrettyPanic $
          interpretLorentzInstr dummyContractEnv instr (Identity initStore :& RNil)
      in case ustoreDecomposeFull dummyUnpackEnv ustore of
          Left err -> assertFailure (toString err)
          Right ustoreHs -> ustoreHs @?= expectedNewStore
