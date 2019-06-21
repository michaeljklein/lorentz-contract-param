module Lorentz.Contracts.UpgradableCounter.V1
  ( UpgradableInterfaceSkeleton(..)
  , migrate
  , counterContract
  , UStoreV1
  ) where

import Lorentz

import Lorentz.Contracts.Upgradable.Common
import Lorentz.UStore

-- Currently ignored
data UStoreTempate = UStoreTempate
  { counterValue :: UStoreField Natural
  } deriving stock (Eq, Generic)

type UStoreV1 = UStore UStoreTempate

-- Currently ignored
data UpgradableInterfaceSkeleton
  = Add Natural
  | Mul Natural
  | GetCounterValue (View () Natural)


runAdd :: '[Natural, UStoreV1] :-> '[([Operation], UStoreV1)]
runAdd = do
  dip $ ustoreGetField #counterValue
  add
  ustoreSetField #counterValue
  nil; pair

runMul :: '[Natural, UStoreV1] :-> '[([Operation], UStoreV1)]
runMul = do
  dip $ ustoreGetField #counterValue
  mul
  ustoreSetField #counterValue
  nil; pair

runView :: '[View () Natural, UStoreV1] :-> '[([Operation], UStoreV1)]
runView = view_ $ do
  cdr
  ustoreGetField #counterValue
  dip drop

-- | This function migrates the storage from an empty one to UStoreV1,
--   i.e. it populates the empty BigMap with entries and initial values
--   for each field. Currently it is not guaranteed that all fields will be set
--   according to the template. See /docs/upgradeableContracts.md for type-safe
--   migrations idea description. The result is expected to adhere
--   to V1.UStoreTemplate.
migrate :: MigrationScript
migrate = do
  coerce_ @UStore_ @UStoreV1
  push @Natural 0
  ustoreSetField #counterValue
  coerce_ @UStoreV1 @UStore_

counterContract :: ContractCode
counterContract = do
  -- This dispatch call (or most probably its substitute) is supposed
  --   to be autogenerated from UpgradableInterfaceSkeleton, or statically
  --   typechecked to prevent incorrect unpacks
  dispatch $
    [ ifArg @Natural [mt|Add|] runAdd
    , ifArg @Natural [mt|Mul|] runMul
    , ifArg @(View () Natural) [mt|GetCounterValue|] runView
    ]
