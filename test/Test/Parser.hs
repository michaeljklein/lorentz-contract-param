module Test.Parser
  ( spec
  ) where

import Data.List (isSuffixOf)
import Morley.Parser as P
import Morley.Types as M
import System.Directory (listDirectory)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (parse)


spec :: Spec
spec = describe "Parser tests" $ do
  it "Successfully parses contracts examples from contracts/" parseContractsTest
  it "Test stringLiteral" stringLiteralTest
  it "IF parsers test" ifParsersTest
  it "MAP parsers test" mapParsersTest
  it "PAIR parsers test" pairParsersTest

parseContractsTest :: Expectation
parseContractsTest = do
  let dir = "contracts"
  files <- (fmap . fmap) (\s -> dir ++ "/" ++ s) $
    fmap (filter (\ x -> (isSuffixOf ".tz" x) || (isSuffixOf ".mtz" x))) $ listDirectory dir
  void $ mapM checkFile files

checkFile :: FilePath -> Expectation
checkFile file = do
  code <- readFile file
  parse P.contract file code `shouldSatisfy` isRight

stringLiteralTest :: Expectation
stringLiteralTest = do
  parse P.stringLiteral "" "\"\"" `shouldSatisfy` isRight
  parse P.stringLiteral "" "\" \\t \\b \\\\  \"" `shouldSatisfy` isRight
  parse P.stringLiteral "" "\"abacaba \\t \\n\\n\\r\"" `shouldSatisfy` isRight
  parse P.stringLiteral "" "\"abacaba \\t \\n\\n\\r a\"" `shouldSatisfy` isLeft
  parse P.stringLiteral "" "\"abacaba \\t \\n\\n\\r" `shouldSatisfy` isLeft

ifParsersTest :: Expectation
ifParsersTest = do
  parse P.ops "" "{IF {} {};}" `shouldBe`
    (Prelude.Right [M.PRIM $ M.IF [] []])
  parse P.ops "" "{IFEQ {} {};}" `shouldBe`
    (Prelude.Right [M.MAC $ M.IFX (M.EQ Nothing) [] []])
  parse P.ops "" "{IFCMPEQ {} {};}" `shouldBe`
    (Prelude.Right [M.MAC $ M.IFCMP (M.EQ Nothing) Nothing [] []])

mapParsersTest :: Expectation
mapParsersTest = do
  parse P.ops "" "{MAP {};}" `shouldBe`
    (Prelude.Right [M.PRIM $ M.MAP Nothing []])
  parse P.ops "" "{MAP_CAR {};}" `shouldBe`
    (Prelude.Right [M.MAC $ M.MAP_CADR [M.A] Nothing Nothing []])

pairParsersTest :: Expectation
pairParsersTest = do
  parse P.ops "" "{PAIR;}" `shouldBe`
    Prelude.Right [M.PRIM $ PAIR Nothing Nothing Nothing Nothing]
  parse P.ops "" "{PAIR %a;}" `shouldBe`
    Prelude.Right [MAC $ PAPAIR (P (F (Nothing,Just "a")) (F (Nothing,Nothing))) Nothing Nothing]
  parse P.ops "" "{PAPAIR;}" `shouldBe`
    Prelude.Right
      [MAC $
        PAPAIR (P (F (Nothing,Nothing)) (P (F (Nothing,Nothing)) (F (Nothing,Nothing))))
          Nothing Nothing
      ]
