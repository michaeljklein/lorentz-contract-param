module Test.Parser
  ( spec
  ) where

import Data.List (isSuffixOf)
import Morley.Parser (contract, stringLiteral)
import System.Directory (listDirectory)
import Test.Hspec (Expectation, Spec, describe, it, shouldSatisfy)
import Text.Megaparsec (parse)


spec :: Spec
spec = describe "Parser tests" $ do
  it "Successfully parses contracts examples from contracts/" parseContractsTest
  it "Test stringLiteral" stringLiteralTest

parseContractsTest :: Expectation
parseContractsTest = do
  let dir = "contracts"
  files <- (fmap . fmap) (\s -> dir ++ "/" ++ s) $
    fmap (filter (\ x -> (isSuffixOf ".tz" x) || (isSuffixOf ".mtz" x))) $ listDirectory dir
  void $ mapM checkFile files

checkFile :: FilePath -> Expectation
checkFile file = do
  code <- readFile file
  parse contract file code `shouldSatisfy` isRight

stringLiteralTest :: Expectation
stringLiteralTest = do
  parse stringLiteral "" "\"\"" `shouldSatisfy` isRight
  parse stringLiteral "" "\" \\t \\b \\\\  \"" `shouldSatisfy` isRight
  parse stringLiteral "" "\"abacaba \\t \\n\\n\\r\"" `shouldSatisfy` isRight
  parse stringLiteral "" "\"abacaba \\t \\n\\n\\r a\"" `shouldSatisfy` isLeft
  parse stringLiteral "" "\"abacaba \\t \\n\\n\\r" `shouldSatisfy` isLeft
