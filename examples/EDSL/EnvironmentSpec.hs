#!/usr/bin/env stack
{- stack
  script
  --resolver snapshot.yaml
  --package base
  --package text
  --package fmt
  --package hspec
  --package QuickCheck
  --package morley
-}

{-
 - © 2019 Tocqueville Group
 -
 - SPDX-License-Identifier: AGPL-3.0-or-later
 -}

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module EnvironmentSpec
  ( spec
  ) where

import Test.Hspec (Spec, hspec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(..), choose)

import Michelson.Interpret (RemainingSteps(..))
import Michelson.Typed (Contract, ToT)
import Michelson.Untyped (UntypedContract, UntypedValue)
import qualified Michelson.Untyped as Untyped
import Morley.Test
  (IntegrationalScenario, TxData(..), expectMichelsonFailed, expectStorageConst, genesisAddress,
  integrationalTestExpectation, originate, setMaxSteps, setNow, specWithContract, transfer,
  validate)
import Tezos.Address (Address, formatAddress)
import Tezos.Core (Mutez, Timestamp, timestampFromSeconds, unsafeMkMutez)

main :: IO ()
main = hspec spec

spec :: Spec
spec = specWithContract "contracts/environment.tz" specImpl

data Fixture = Fixture
  { fNow :: !Timestamp
  , fMaxSteps :: !RemainingSteps
  , fPassOriginatedAddress :: !Bool
  , fBalance :: !Mutez
  , fAmount :: !Mutez
  } deriving (Show)

instance Arbitrary Fixture where
  arbitrary = do
    fNow <- timestampFromSeconds <$> choose (100000 :: Int, 111111)
    fMaxSteps <- RemainingSteps <$> choose (1015, 1028)
    fPassOriginatedAddress <- arbitrary
    fBalance <- unsafeMkMutez <$> choose (1, 1234)
    fAmount <- unsafeMkMutez <$> choose (1, 42)
    return Fixture {..}

shouldExpectFailed :: Fixture -> Bool
shouldExpectFailed fixture =
  or
    [ fBalance fixture > unsafeMkMutez 1000
    , fNow fixture < timestampFromSeconds (100500 :: Int)
    , fPassOriginatedAddress fixture
    , fAmount fixture < unsafeMkMutez 15
    ]

shouldReturn :: Fixture -> UntypedValue
shouldReturn fixture
  | fMaxSteps fixture - consumedGas > 1000 = Untyped.ValueTrue
  | otherwise = Untyped.ValueFalse
  where
    consumedGas = 19

specImpl ::
    (UntypedContract, Contract (ToT Address) (ToT Bool))
  -> Spec
specImpl (uEnvironment, _environment)  = do
  let scenario = integrationalScenario uEnvironment
  prop description $
    integrationalTestExpectation . scenario
  where
    description =
      "This contract fails under conditions described in a comment at the " <>
      "beginning of this contract and returns whether remaining gas is " <>
      "greater than 1000"

integrationalScenario :: UntypedContract -> Fixture -> IntegrationalScenario
integrationalScenario contract fixture = do
  -- First of all let's set desired gas limit and NOW
  setNow $ fNow fixture
  setMaxSteps $ fMaxSteps fixture

  -- Then let's originated the 'environment.tz' contract
  environmentAddress <-
    originate contract Untyped.ValueFalse (fBalance fixture)

  -- And transfer tokens to it
  let
    param
      | fPassOriginatedAddress fixture = environmentAddress
      | otherwise = genesisAddress
    txData = TxData
      { tdSenderAddress = genesisAddress
      , tdParameter = Untyped.ValueString (formatAddress param)
      , tdAmount = fAmount fixture
      }
  transfer txData environmentAddress

  -- Execute operations and check that interpreter fails when one of
  -- failure conditions is met or updates environment's storage
  -- approriately
  let
    validator
      | shouldExpectFailed fixture =
        Left $ expectMichelsonFailed environmentAddress
      | otherwise =
        Right $ expectStorageConst environmentAddress $ shouldReturn fixture
  validate validator
