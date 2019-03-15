module Auction where

import Lorentz
import qualified Lorentz as L

type Parameter = TKeyHash

type AuctionEnd = TTimestamp
type Bid = TPair TMutez TKeyHash

type Storage = TPair AuctionEnd Bid

type Input = TPair Parameter Storage
type Output storage = TPair (TList TOperation) storage

cdar = cdr # car
cddr = cdr # cdr
fail_ = unit # failWith

code :: Contract Parameter Storage
code = checkIfAuctionHasEnded #
       setupReplacementStorage #
       checkNewBidIsGreater #
       getRefund #
       makeRefund #
       callingConvention

checkIfAuctionHasEnded :: '[ Input ] +> '[ Input, TTimestamp]
checkIfAuctionHasEnded = dup # cdar # dup # now # gt # if_ fail_ nop # L.swap

setupReplacementStorage :: '[ Input, TTimestamp] +> '[ Bid, Storage ]
setupReplacementStorage =
  dup # car # dip0 cddr # amount # pair # L.swap # dip0 (L.swap # pair)

checkNewBidIsGreater :: '[ Bid, Storage ] +> '[ Bid, Storage ]
checkNewBidIsGreater = dup # car # amount # le # if_ fail_ nop

getRefund :: '[ Bid, Storage ] +> '[ TMutez, Bid, Storage ]
getRefund = dup # car

makeRefund :: '[ TMutez, Bid, Storage ] +> '[ TOperation, Storage ]
makeRefund = dip0 (cdr # implicitAccount) # unit # transferTokens

callingConvention :: '[ TOperation, Storage ] +> '[ Output Storage ]
callingConvention = nil # L.swap # cons # pair


