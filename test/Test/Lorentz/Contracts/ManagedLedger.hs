{-# LANGUAGE DeriveAnyClass, DerivingStrategies, NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.Lorentz.Contracts.ManagedLedger
  ( Parameter (..)
  , Storage (..)
  , managedLedger
  ) where

import Lorentz
import Prelude (Maybe(..))

data Parameter
  = Transfer TransferParams
  | Mint MintParams
  | Burn BurnParams
  | SetApprover (Maybe Address)
  | SetManager Address
  | GetApprover (View () (Maybe Address))
  | GetManager  (View () Address)
  | GetTotalSupply (View () Natural)
  | GetBalance     (View Address (Maybe Natural))
  deriving stock Generic
  deriving anyclass IsoValue

type TransferParams = ("from" :! Address, "to" :! Address, "val" :! Natural)
type MintParams = ("to" :! Address, "val" :! Natural)
type BurnParams = ("from" :! Address, "val" :! Natural)

data Storage = Storage
  { ledger      :: BigMap Address Natural
  , totalSupply :: Natural
  , approver :: Maybe Address
  , manager :: Address
  }
  deriving stock Generic
  deriving anyclass IsoValue

managedLedger :: Contract Parameter Storage
managedLedger = do
  unpair
  caseT @Parameter
    ( #cTransfer /-> do
        dip (do get_ #approver; assertSome; sender; eq; assert;)
        debitFrom; creditTo; nil; pair
    , #cMint /-> do
        dip (do get_ #manager; source; eq; assert;)
        creditTo'; nil; pair
    , #cBurn /-> do
        dip (do get_ #manager; source; eq; assert;)
        debitFrom'; drop; nil; pair
    , #cSetApprover /-> do set_ #approver; nil; pair;
    , #cSetManager /-> do set_ #manager; nil; pair;
    , #cGetApprover /-> do view_ (do cdr; access_ #approver)
    , #cGetManager /-> do view_ (do cdr; access_ #manager)
    , #cGetTotalSupply /-> do view_ (do cdr; access_ #totalSupply)
    , #cGetBalance /-> do view_ (do unpair; dip ( access_ #ledger ); get)
    )


debitFrom :: '[TransferParams, Storage] :-> '[TransferParams, Storage]
debitFrom = do
  get_ #from; swap;
  dip (do dip (get_ #ledger); get; assertSome); swap
  dip (get_ #val);
  pair; apply_ subGt0; swap;
  get_ #from; swap;
  dip (do dipX @2 (get_ #ledger); update; set_ #ledger)

debitFrom' :: '[BurnParams, Storage] :-> '[BurnParams, Storage]
debitFrom' = do
  get_ #from; swap;
  dip (do dip (get_ #ledger); get; assertSome); swap
  dip (get_ #val);
  pair; apply_ subGt0; swap;
  get_ #from; swap;
  dip (do dipX @2 (get_ #ledger); update; set_ #ledger)

creditTo :: '[TransferParams, Storage] :-> '[Storage]
creditTo = do
  get_ #to; swap
  dip (do dip (get_ #ledger); get); swap
  if IsSome then dip (get_ #val) >> add @Natural else get_ #val
  some >> dip (access_ #to); swap;
  dipX @2 (get_ #ledger)
  update;
  set_ #ledger

creditTo' :: '[MintParams, Storage] :-> '[Storage]
creditTo' = do
  get_ #to; swap
  dip (do dip (get_ #ledger); get); swap
  if IsSome then dip (get_ #val) >> add @Natural else get_ #val
  some >> dip (access_ #to); swap;
  dipX @2 (get_ #ledger)
  update;
  set_ #ledger

subGt0 :: Lambda (Natural, Natural) (Maybe Natural)
subGt0 = do
  unpair; sub;
  dup; assertGe0
  dup; eq0
  if Holds
  then drop >> none
  else isNat



