{-
 - © 2019 Tocqueville Group
 -
 - SPDX-License-Identifier: AGPL-3.0-or-later
 -}

-- | Module, containing spec to test conditionals.tz contract.
module Test.Interpreter.Conditionals
  ( conditionalsSpec
  ) where

import Test.Hspec (Spec, it, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Property (withMaxSuccess)

import Michelson.Interpret (InterpreterState, MichelsonFailed)
import Michelson.Typed (CValue(..), ToT)
import qualified Michelson.Typed as T
import Morley.Test (contractProp, specWithTypedContract)
import Morley.Test.Dummy (dummyContractEnv)
import Morley.Test.Util (failedProp, qcIsLeft, qcIsRight)
import Morley.Types (MorleyLogs)

type Param = Either Text (Maybe Integer)
type ContractStorage = T.Value (ToT Text)
type ContractResult x
   = ( Either MichelsonFailed ([x], ContractStorage)
     , InterpreterState MorleyLogs)

-- | Spec to test conditionals.tz contract.
conditionalsSpec :: Spec
conditionalsSpec = parallel $ do

  specWithTypedContract "contracts/conditionals.tz" $ \contract -> do
    let
      contractProp' inputParam =
        contractProp contract (validate inputParam) dummyContractEnv inputParam
        ("storage" :: Text)

    it "success 1 test" $
      contractProp' $ Left "abc"

    prop "Random check" $ withMaxSuccess 200 contractProp'
  where
    validate
      :: Show x
      => Param
      -> ContractResult x
      -> Property
    validate (Left a) (Right ([], T.VC (CvString b)), _) = a === b
    validate (Right Nothing) r = qcIsLeft $ fst r
    validate (Right (Just a)) r
      | a < 0 = qcIsLeft $ fst r
      | otherwise = qcIsRight $ fst r
    validate _ res = failedProp $ "Unexpected result: " <> show res
