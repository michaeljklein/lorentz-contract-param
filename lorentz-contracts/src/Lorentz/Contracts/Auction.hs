module Lorentz.Contracts.Auction
  ( auction
  ) where

import Lorentz

data Storage = Storage
  { auctionEnd    :: Timestamp
  , highBidAmount :: Mutez
  , highBidFrom   :: KeyHash
  }
  deriving stock Generic
  deriving anyclass IsoValue

auction :: Contract KeyHash Storage
auction = do
  unpair
  dip (do get_ #auctionEnd; now; assertLe)
  dip (do get_ #highBidAmount; amount; assertGt)
  set_ #highBidFrom
  get_ #highBidFrom; implicitAccount;
  dip (do get_ #highBidAmount); swap; unit
  transferTokens; nil; swap; cons
  dip (do amount; set_ #highBidAmount); pair
