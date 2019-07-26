module Test.Util.Columnar
  ( test_columnarUpdate
  ) where

import Test.HUnit ((@?=))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Util.Columnar

data Template1 f = Template1
  { tNumber1 :: f -/ Integer
  , tNumber2 :: f -/ Integer
  , tTemplate2 :: Template2 f
  , tDummy :: ()
  } deriving Generic

deriving stock instance Eq (Template1 Identity)
deriving stock instance Eq (Template1 Maybe)
deriving stock instance Show (Template1 Identity)
deriving stock instance Show (Template1 Maybe)

data Template2 f = Template2
  { tText :: f -/ Text
  } deriving Generic

deriving stock instance Eq (Template2 Identity)
deriving stock instance Eq (Template2 Maybe)
deriving stock instance Show (Template2 Identity)
deriving stock instance Show (Template2 Maybe)

test_columnarUpdate :: [TestTree]
test_columnarUpdate =
    [ testCase "Update via columnars works" $
        columnarUpdate
          Template1
          { tNumber1 = 1
          , tNumber2 = 2
          , tTemplate2 = Template2 ""
          , tDummy = ()
          }
          Template1
          { tNumber1 = Nothing
          , tNumber2 = Just 7
          , tTemplate2 = Template2 (Just "ab")
          , tDummy = ()
          }
        @?=
        Template1
          { tNumber1 = 1
          , tNumber2 = 7
          , tTemplate2 = Template2 "ab"
          , tDummy = ()
          }

    , testCase "Constructing empty update for columnars works" $
        columnarEmptyUpdate
        @?=
        Template1
          { tNumber1 = Nothing
          , tNumber2 = Nothing
          , tTemplate2 = Template2 Nothing
          , tDummy = ()
          }
    ]
