-- | Module, containing spec to FA1.2.

module Test.Lorentz.Contracts.ManagedLedger
  ( spec_ManagedLedger
  ) where

-- Import Prelude to make intero calm
import Prelude

import qualified Data.Map as Map
import Fmt ((+||), (||+))
import Test.Hspec (Spec, describe, it)

import Lorentz (View(..))
import Lorentz.Contracts.Consumer
import Lorentz.Contracts.ManagedLedger
import Lorentz.Test
import Lorentz.Value
import Tezos.Address (Address)
import Util.Instances ()
import Util.Named ((.!))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

wallet1, wallet2, wallet3, admin, admin2 :: Address
wallet1 = genesisAddress1
wallet2 = genesisAddress2
wallet3 = genesisAddress3
admin = genesisAddress4
admin2 = genesisAddress5

-- | Originate the contract we are currently testing with empty storage.
originateEmptyManagedLedger :: IntegrationalScenarioM (ContractAddr Parameter)
originateEmptyManagedLedger =
  lOriginate managedLedgerContract "Managed ledger"
    (mkStorage admin) (toMutez 1000)

-- | Originate the contract we are currently testing with given amount of money
-- on some accounts.
originateManagedLedger
  :: Natural
  -> IntegrationalScenarioM (ContractAddr Parameter)
originateManagedLedger initMoney =
  lOriginate managedLedgerContract "Managed ledger"
    (mkStorage admin)
    { ledger = BigMap $ Map.fromList
        [ (wallet1, (#balance .! initMoney, #approvals .! mempty))
        , (wallet2, (#balance .! initMoney, #approvals .! mempty))
        ]
    }
    (toMutez 1000)

-- Call managed ledger with sane parameter
lCallSane :: ContractAddr Parameter -> SaneParameter -> IntegrationalScenarioM ()
lCallSane addr = lCall addr . fromSaneParameter

spec_ManagedLedger :: Spec
spec_ManagedLedger = trace "spec" $ do
  describe "Transfers authorized by admin" $ do
    it "Can mint money" $
      integrationalTestExpectation $ do
        ml <- originateEmptyManagedLedger

        validate . Right $ expectAnySuccess
