{-# LANGUAGE DeriveAnyClass, DerivingStrategies, NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.Lorentz.Contracts.UnsafeLedger
  ( Parameter (..)
  , Storage (..)
  , unsafeLedger
  ) where

import Lorentz
import Prelude (Maybe(..))

data Parameter
  = Transfer TransferParams
  | GetTotalSupply (View () Natural)
  | GetBalance     (View Address (Maybe Natural))
  deriving stock Generic
  deriving anyclass IsoValue

type TransferParams = ("to" :! Address, "val" :! Natural)
data Storage = Storage
  { ledger      :: BigMap Address Natural
  , totalSupply :: Natural
  }
  deriving stock Generic
  deriving anyclass IsoValue

unsafeLedger :: Contract Parameter Storage
unsafeLedger = do
  unpair
  caseT @Parameter
    ( #cTransfer /-> do debitSource; creditTo; nil; pair;
    , #cGetTotalSupply /-> view_ (do cdr; access_ #totalSupply)
    , #cGetBalance /-> view_ (do unpair; dip ( access_ #ledger ); get)
    )

debitSource :: '[TransferParams, Storage]
            :-> '[TransferParams, Storage]
debitSource = do
  dip (do get_ #ledger; source; get; assertSome)
  swap
  dip (get_ #val);
  pair; apply_ subGt0
  swap;
  dip (do dip (get_ #ledger); source; update; set_ #ledger)

creditTo :: '[TransferParams, Storage] :-> '[Storage]
creditTo = do
  get_ #to
  swap
  dip (do dip (get_ #ledger); get)
  swap
  if IsSome then dip (get_ #val) >> add @Natural else get_ #val
  some
  dip (access_ #to)
  swap;
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



