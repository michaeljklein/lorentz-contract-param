
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

{- | Direct port of the Michelson gas type and basic calculations.
-}

module Gas.Type
  ( -- * Cost
    Cost
  , allocCost
  , allocBytesCost
  , allocBitsCost
  , allocMBytesCost
  , stepCost
  , readBytesCost
  , writeBytesCost
  , free
  , scale

    -- * Gas
  , RemainingGas (..)
  , consumeGas
  )
  where

import Data.Default
import Fmt

import Tezos.Core

data Cost = Cost
  { gcAllocations  :: Word64
  , gcSteps        :: Word64
  , gcReads        :: Word64
  , gcWrites       :: Word64
  , gcBytesRead    :: Word64
  , gcBytesWritten :: Word64
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (Default, Buildable)

instance Semigroup Cost where
  cost1 <> cost2 = Cost
    { gcAllocations  = gcAllocations  cost1 + gcAllocations  cost2
    , gcSteps        = gcSteps        cost1 + gcSteps        cost2
    , gcReads        = gcReads        cost1 + gcReads        cost2
    , gcWrites       = gcWrites       cost1 + gcWrites       cost2
    , gcBytesRead    = gcBytesRead    cost1 + gcBytesRead    cost2
    , gcBytesWritten = gcBytesWritten cost1 + gcBytesWritten cost2
    }

instance Monoid Cost where
  mempty = def

data RemainingGas
  = Unaccounted
  | Limited Mutez
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (Buildable)

allocationPrice  :: Mutez
stepPrice        :: Mutez
readBasePrice    :: Mutez
writeBasePrice   :: Mutez
byteReadPrice    :: Mutez
byteWrittenPrice :: Mutez
zero             :: Mutez

allocationPrice  = unsafeMkMutez 2
stepPrice        = unsafeMkMutez 1
readBasePrice    = unsafeMkMutez 50
writeBasePrice   = unsafeMkMutez 80
byteReadPrice    = unsafeMkMutez 10
byteWrittenPrice = unsafeMkMutez 15
zero             = unsafeMkMutez 0

consumeGas
  :: e
  -> e
  -> e
  -> Mutez
  -> RemainingGas
  -> Cost
  -> Either e (Mutez, RemainingGas)
consumeGas
  blockQuotaExceeded
  operationQuotaExceeded
  mutezOverflow
  blockGas
  operationGas
  cost
    =
  case operationGas of
    Unaccounted -> do
      return (blockGas, Unaccounted)

    Limited remaining -> do
      do
        weightedCost <- asEither mutezOverflow $ do
          allocs     <- mulMutez allocationPrice  (gcAllocations  cost)
          steps      <- mulMutez stepPrice        (gcSteps        cost)
          readBase   <- mulMutez readBasePrice    (gcReads        cost)
          writeBase  <- mulMutez writeBasePrice   (gcWrites       cost)
          readBytes  <- mulMutez byteReadPrice    (gcBytesRead    cost)
          writeBytes <- mulMutez byteWrittenPrice (gcBytesWritten cost)

          foldM addMutez zero [allocs, steps, readBase, writeBase, readBytes, writeBytes]

        remaining' <- asEither operationQuotaExceeded $ do
          remaining `subMutez` weightedCost

        blockRemaining <- asEither blockQuotaExceeded $ do
          blockGas `subMutez` weightedCost

        return (blockRemaining, Limited remaining')

asEither e = maybe (Left e) Right

allocCost      :: Word64 -> Cost
allocBytesCost :: Word64 -> Cost
allocBitsCost  :: Word64 -> Cost
stepCost       :: Word64 -> Cost
readBytesCost  :: Word64 -> Cost
writeBytesCost :: Word64 -> Cost
free           :: Cost

allocCost      n = def { gcAllocations  =  n + 1 }
allocBytesCost n = def { gcAllocations  = (n + 7)  `div` 8 }
allocBitsCost  n = def { gcAllocations  = (n + 63) `div` 64 }
stepCost       n = def { gcSteps        =  n }
readBytesCost  n = def { gcBytesRead    =  n }
writeBytesCost n = def { gcBytesWritten =  n }
free             = def

allocMBytesCost :: Word64 -> Cost
allocMBytesCost n = allocCost 12 <> allocBytesCost n

scale :: Word64 -> Cost -> Cost
scale n cost = Cost
    { gcAllocations  = n * gcAllocations  cost
    , gcSteps        = n * gcSteps        cost
    , gcReads        = n * gcReads        cost
    , gcWrites       = n * gcWrites       cost
    , gcBytesRead    = n * gcBytesRead    cost
    , gcBytesWritten = n * gcBytesWritten cost
    }
