module Main where

import Test.Hspec (Expectation, shouldBe)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

import Michelson.Typed

e :: Expectation
e = do
  let x = -64 :: Integer
      x' = VC (CvInt -63)
  toVal x `shouldBe` x'

main :: IO ()
main = T.defaultMain $ HU.testCase "e" e
