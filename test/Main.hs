module Main where

import Test.HUnit (Assertion, (@?=))
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

import Michelson.Typed.Value

e :: Assertion
e = do
  let x = -64 :: Integer
      x' = VC (CvInt -63)
  toVal x @?= x'

main :: IO ()
main = T.defaultMain $ HU.testCase "e" e
