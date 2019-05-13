{-# LANGUAGE DeriveAnyClass, DerivingStrategies, NoApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.Lorentz.Contracts.Approvals
  ( Parameter (..)
  , Storage (..)
  , approvals
  ) where

import Lorentz
import Prelude (Maybe(..))

data Parameter
  = TransferFrom TransferParams
  | Approve TransferParams
  | GetAllowance (View GetAllowanceParams Natural)
  deriving stock Generic
  deriving anyclass IsoValue

type TransferParams =("from" :! Address, "to" :! Address, "val" :! Natural)
type GetAllowanceParams = ("from" :! Address, "to" :! Address)

data Storage = Storage
  { approvalsMap  :: BigMap Address (Map Address Natural)
  , ledgerAddress :: Address
  }
  deriving stock Generic
  deriving anyclass IsoValue

approvals :: Contract Parameter Storage
approvals = do
  unpair
  caseT @Parameter
    ( #cTransferFrom /-> do
        get_ #to; source; assertEq
        duupX @2; duupX @2; allowance
        dip (get_ #val); assertEq
        dup; dip (do push 0; set_ #val; setAllowance)
        dip (do get_ #ledgerAddress; contract @TransferParams; assertSome)
        dip (amount); transferTokens
        nil; swap; cons; pair;
    , #cApprove /-> do
        get_ #from; source; assertEq
        duupX @2; duupX @2; allowance
        eq0; if_ nop (do get_ #val; assertEq0)
        setAllowance; nil; pair
    , #cGetAllowance /-> view_ (unpair >> allowance)
    )

zeroNone :: (Natural ': s) :-> (Maybe Natural ': s)
zeroNone = do dup; eq0; if_ (drop >> none) some

allowance :: (HasFieldOfType a "from" Address, HasFieldOfType a "to" Address)
          => a ': Storage ': s :-> Natural ': s
allowance = do
  get_ #from; swap
  dip (do dip (access_ #approvalsMap); get; assertSome)
  access_ #to; get; ifNone (push 0) nop

setAllowance :: (TransferParams ': Storage ': s)
             :-> (Storage ': s)
setAllowance = do
  dip (get_ #approvalsMap); swap;
  dip (get_ #from); swap;
  get; ifNone emptyMap nop;
  dip (get_ #to); swap;
  dip (do dip (get_ #val); swap; zeroNone)
  update; some
  dip (do access_ #from; dip (get_ #approvalsMap)); swap;
  update;
  set_ #approvalsMap




