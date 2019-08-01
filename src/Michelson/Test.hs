-- | Module containing some utilities for testing Michelson contracts using
-- Haskell testing frameworks (hspec and QuickCheck in particular).
-- It's Morley testing EDSL.

module Michelson.Test
  ( -- * Importing a contract
    specWithContract
  , specWithContractL
  , specWithTypedContract
  , specWithUntypedContract
  , testTreesWithContract
  , testTreesWithContractL
  , testTreesWithTypedContract
  , concatTestTrees
  , importUntypedContract

  -- * Unit testing
  , ContractReturn
  , ContractPropValidator
  , contractProp
  , contractPropVal
  , contractRepeatedProp
  , contractRepeatedPropVal

  -- * Integrational testing
  -- ** Testing engine
  , IntegrationalValidator
  , SuccessValidator
  , IntegrationalScenario
  , IntegrationalScenarioM
  , integrationalTestExpectation
  , integrationalTestProperty
  , originate
  , transfer
  , validate
  , setMaxSteps
  , setNow
  , branchout
  , (?-)

  -- ** Validators
  , composeValidators
  , composeValidatorsList
  , expectAnySuccess
  , expectNoUpdates
  , expectNoStorageUpdates
  , expectStorageUpdate
  , expectStorageUpdateConst
  , expectBalance
  , expectStorageConst
  , expectGasExhaustion
  , expectMichelsonFailed

  -- ** Various
  , TxData (..)
  , genesisAddress

  -- * General utilities
  , failedProp
  , succeededProp
  , qcIsLeft
  , qcIsRight

  -- * Dummy values
  , dummyContractEnv

  -- * Arbitrary data
  , minTimestamp
  , maxTimestamp
  , midTimestamp
  ) where

import Michelson.Test.Dummy
import Michelson.Test.Gen
import Michelson.Test.Import
import Michelson.Test.Integrational
import Michelson.Test.Unit
import Michelson.Test.Util
