module Test.Serialization.Michelson
  ( unit_Packing
  ) where

import Prelude hiding (Ordering(..))

import Data.Singletons (SingI(..))
import qualified Data.Text as T
import Data.Typeable ((:~:)(..), Typeable, eqT, typeRep)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Text.Hex (encodeHex)

import Michelson.Interpret.Pack (packValue')
import Michelson.Macro (expandList)
import qualified Michelson.Parser as Parser
import Michelson.Test.Util
import Michelson.TypeCheck (HST(..), SomeInstr(..), SomeInstrOut(..), typeCheckList)
import Michelson.Typed
import Michelson.Untyped (noAnn)
import Test.Util.Parser

unit_Packing :: Expectation
unit_Packing = do
  let x = -64 :: Integer
      h = "0500c001" :: Text
      x' = VC (CvInt -63)
      x'' = VC $ toCVal x
  -- encodeHex (packValue' $ x') `shouldBe` h
  -- encodeHex (packValue' $ x'') `shouldBe` h
  encodeHex (packValue' $ toVal x) `shouldBe` h
