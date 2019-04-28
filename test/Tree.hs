module Tree where
import Prelude
import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Hspec as HS
import qualified Test.Tasty.Ingredients as T


import qualified Test.Tasty.HUnit as HU

import qualified Test.Parser

import qualified Test.Macro

import qualified Test.Interpreter

import qualified Test.CValConversion

import qualified Test.Typecheck

import qualified Test.Ext

import qualified Test.ValConversion

import qualified Test.Tezos.Crypto

import qualified Test.Tezos.Address

import qualified Test.Michelson.Runtime

import qualified Test.Printer.Michelson

import qualified Test.Serialization.Michelson

import qualified Test.Serialization.Aeson


class TestGroup a where testGroup :: String -> a -> IO T.TestTree
instance TestGroup T.TestTree        where testGroup _ a = pure a
instance TestGroup [T.TestTree]      where testGroup n a = pure $ T.testGroup n a
instance TestGroup (IO T.TestTree)   where testGroup _ a = a
instance TestGroup (IO [T.TestTree]) where testGroup n a = T.testGroup n <$> a

class TestCase a where testCase :: String -> a -> IO T.TestTree
instance TestCase (IO ())                      where testCase n = pure . HU.testCase      n
instance TestCase (IO String)                  where testCase n = pure . HU.testCaseInfo  n
instance TestCase ((String -> IO ()) -> IO ()) where testCase n = pure . HU.testCaseSteps n

tests :: IO T.TestTree
tests = do
  t45 <- testCase "Packing" Test.Serialization.Michelson.unit_Packing

  pure $ T.testGroup "test/Tree.hs"
    [T.testGroup "Test"
      [ T.testGroup "CValConversion" []--t23,t24,t25]
--      , T.testGroup "Ext" [t31,t32]
--      , T.testGroup "Interpreter" [t22]
--      , T.testGroup "Macro" [t14,t15,t16,t17,t18,t19,t20,t21]
--      , T.testGroup "Michelson.Runtime" [t41]
--      , T.testGroup "Parser" [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13]
--      , T.testGroup "Printer.Michelson" [t42]
      , T.testGroup "Serialization" [T.testGroup "Aeson" [],T.testGroup "Michelson" [t45]]
--      , T.testGroup "Tezos" [T.testGroup "Address" [t39,t40],T.testGroup "Crypto" [t35,t36,t37,t38]],T.testGroup "Typecheck" [t26,t27,t28,t29,t30],T.testGroup "ValConversion" [t33,t34]
      ]
    ]
