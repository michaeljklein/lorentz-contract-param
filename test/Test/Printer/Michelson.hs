module Test.Printer.Michelson
  ( spec
  ) where

import Prelude hiding (bool)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import Michelson.Printer (printUntypedContract)
import Morley.Test (readUntypedContract, specWithUntypedContract)

import Test.Util.Contracts (getWellTypedContracts)

spec :: Spec
spec = describe "Michelson.TzPrinter.printUntypedContract" $ do
  contractFiles <- runIO getWellTypedContracts
  mapM_ roundtripPrintTest contractFiles

roundtripPrintTest :: FilePath -> Spec
roundtripPrintTest filePath =
  -- these are untyped and expanded contracts, they might have macros
  specWithUntypedContract filePath $ \contract1 ->
    it "roundtrip printUntypedContract test" $ do
      case readUntypedContract filePath (toText $ printUntypedContract contract1) of
        Left err -> fail ("Failed to read 'printUntypedContract contract1': " ++ show err)
        Right contract2 ->
          printUntypedContract contract1 `shouldBe` printUntypedContract contract2
