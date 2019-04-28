{-
 - © 2019 Tocqueville Group
 - © 2016 IOHK
 -        (original license: LicenseRef-MIT-IOHK)
 -
 - SPDX-License-Identifier: AGPL-3.0-or-later
 -}

-- | Testing utilities to be used with QuickCheck

module Test.Util.QuickCheck
  ( ShowThroughBuild (..)

  -- * Roundtrip properties
  , roundtripSpec
  , roundtripSpecSTB
  , aesonRoundtrip

  -- * 'Gen' helpers
  , runGen
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import Data.Typeable (typeRep)
import Fmt (Buildable, pretty)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Property, (===))
import Test.QuickCheck.Gen (Gen, unGen)
import Test.QuickCheck.Random (mkQCGen)
import qualified Text.Show (show)

----------------------------------------------------------------------------
-- 'Show'ing a value though 'Buildable' type class.
-- Useful because QuickCheck uses 'Show'.
----------------------------------------------------------------------------

newtype ShowThroughBuild a = STB
  { unSTB :: a
  } deriving (Eq, Ord, Arbitrary)

instance {-# OVERLAPPABLE #-} Buildable a => Show (ShowThroughBuild a) where
  show = pretty . unSTB

instance Show (ShowThroughBuild ByteString) where
  show = show . unSTB

----------------------------------------------------------------------------
-- Formatting
----------------------------------------------------------------------------

-- | This 'Spec' contains a property based test for conversion from
-- some @x@ to some @y@ and back to @x@ (it should successfully return
-- the initial @x@).
roundtripSpec ::
     forall x y err.
     ( Show x
     , Typeable x
     , Arbitrary x
     , Eq x
     , Show err
     , Eq err
     )
  => (x -> y)
  -> (y -> Either err x)
  -> Spec
roundtripSpec xToY yToX = prop typeName check
  where
    typeName = show $ typeRep (Proxy @x)
    check :: x -> Property
    check x = yToX (xToY x) === Right x

-- | Version of 'roundtripSpec' which shows values using 'Buildable' instance.
roundtripSpecSTB ::
     forall x y err.
     ( Show (ShowThroughBuild x)
     , Typeable x
     , Arbitrary x
     , Eq x
     , Show (ShowThroughBuild err)
     , Eq err
     )
  => (x -> y)
  -> (y -> Either err x)
  -> Spec
roundtripSpecSTB xToY yToX = roundtripSpec (xToY . unSTB) (bimap STB STB . yToX)

aesonRoundtrip ::
     forall x. (Show (ShowThroughBuild x), ToJSON x, FromJSON x, Typeable x, Arbitrary x, Eq x)
  => Spec
aesonRoundtrip = roundtripSpecSTB (Aeson.encode @x) Aeson.eitherDecode

----------------------------------------------------------------------------
-- Gen
----------------------------------------------------------------------------

-- | Get something out of a quickcheck 'Gen' without having to do IO
runGen :: Gen a -> a
runGen g = unGen g (mkQCGen 31415926) 30
