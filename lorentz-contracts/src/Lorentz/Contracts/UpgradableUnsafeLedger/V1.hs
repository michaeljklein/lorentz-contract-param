-- | A buggy implementation of Unsafe ledger, returns balances multiplied by 2

module Lorentz.Contracts.UpgradableUnsafeLedger.V1
  ( UpgradableInterfaceSkeleton(..)
  , migrate
  , unsafeLedgerContract

  -- The following are used in V2
  , UStoreV1
  , TransferParams
  , transfer
  , getTotalSupply
  ) where

import Lorentz

import Lorentz.UStore
import Lorentz.Contracts.Upgradable.Common

-- Currently ignored
data UpgradableInterfaceSkeleton
  = Transfer TransferParams
  | GetTotalSupply (View () Natural)
  | GetBalance     (View Address (Maybe Natural))

-- We lose arg names here but I believe it's possible to bring them back
-- in generic implementation
type TransferParams = (Address, Natural)

data UStoreTempate = UStoreTempate
  { ledger      :: Address |~> Natural
  , totalSupply :: UStoreField Natural
  } deriving stock (Eq, Generic)

type UStoreV1 = UStore UStoreTempate

-- | Like in UpgradableCounter, this function  populates the empty UStore_
--   with entries and initial values for each field. The result is expected
--   to adhere to V1.UStoreTemplate
migrate :: '[UStore_] :-> '[UStore_]
migrate = do
  coerce_ @UStore_ @UStoreV1
  push @Natural 500
  dup
  dip $ ustoreSetField #totalSupply
  sender
  ustoreInsert #ledger
  coerce_ @UStoreV1 @UStore_

unsafeLedgerContract :: ContractCode
unsafeLedgerContract = do
  dispatch
    [ ifArg @TransferParams [mt|Transfer|] transfer
    , ifArg @(View () Natural) [mt|GetTotalSupply|] getTotalSupply
    , ifArg @(View Address (Maybe Natural)) [mt|GetBalance|] buggyGetBalance
    ]

transfer :: '[TransferParams, UStoreV1]
         :-> '[([Operation], UStoreV1)]
transfer = do
  debitSource; creditTo; nil; pair;

getTotalSupply :: '[View () Natural, UStoreV1]
               :-> '[([Operation], UStoreV1)]
getTotalSupply = view_ (do cdr; ustoreToField #totalSupply)

-- Buggy getBalance returns balance multiplied by 2
buggyGetBalance :: '[View Address (Maybe Natural), UStoreV1]
                :-> '[([Operation], UStoreV1)]
buggyGetBalance = view_ $ do
  unpair
  ustoreGet #ledger
  if IsSome
  then push @Natural 2 >> mul >> some
  else none

debitSource :: '[TransferParams, UStoreV1]
            :-> '[TransferParams, UStoreV1]
debitSource = do
  dip $ do
    sender
    dip dup
    ustoreGet #ledger
    assertSome [mt|Sender address is not in ledger|]
  swap
  dip (dup # cdr)
  subGt0
  swap
  dip (do sender; ustoreUpdate #ledger)

creditTo :: '[TransferParams, UStoreV1] :-> '[UStoreV1]
creditTo = do
  dup; car
  swap
  dip (dip dup # ustoreGet #ledger)
  swap
  if IsSome then dip (dup >> cdr) >> add @Natural else (dup >> cdr)
  some
  dip (car)
  swap
  ustoreUpdate #ledger

subGt0 :: Natural ': Natural ': s :-> Maybe Natural ': s
subGt0 = do
  sub;
  dup; assertGe0 [mt|Transferred value is greater than balance|]
  dup; eq0
  if Holds
  then drop >> none
  else isNat
