{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module, providing singleton boilerplate for
-- 'T' and 'CT' data types.
--
-- Some functions from Data.Singletons are provided alternative version here.
-- Some instances which are usually generated with TH are manually implemented
-- as they require some specific constraints, namely 'Typeable' and/or
-- 'Converge', not provided in instances generated by TH.

module Michelson.Typed.Sing
  ( Sing (..)
  , withSomeSingT
  , withSomeSingCT
  , fromSingT
  , fromSingCT
  , GetDict(..)
  ) where

import Data.Kind (Type)
import Data.Singletons (Sing(..), SingI(..), SingKind(..), SomeSing(..))
import Data.Constraint
import GHC.TypeLits
import Util.TypeLits

import Michelson.Typed.T (EraseNotes, TFieldAnn(..), CT(..), T(..))

-- | Instance of data family 'Sing' for 'CT'.
data instance Sing :: CT -> Type where
  SCInt :: Sing  'CInt
  SCNat :: Sing  'CNat
  SCString :: Sing  'CString
  SCBytes :: Sing  'CBytes
  SCMutez :: Sing  'CMutez
  SCBool :: Sing  'CBool
  SCKeyHash :: Sing  'CKeyHash
  SCTimestamp :: Sing  'CTimestamp
  SCAddress :: Sing  'CAddress

-- | Instance of data family 'Sing' for 'T'.
-- Custom instance is implemented in order to inject 'Typeable'
-- constraint for some of constructors.
data instance Sing :: T -> Type where
  STc :: (SingI a, Typeable a) => Sing a -> Sing ( 'Tc a)
  STKey :: Sing  'TKey
  STUnit :: Sing  'TUnit
  STSignature :: Sing  'TSignature
  STOption :: (SingI a, Typeable a) => Sing a -> Sing ( 'TOption a)
  STList :: (SingI a, Typeable a) => Sing a -> Sing ( 'TList a )
  STSet :: (SingI a, Typeable a) => Sing a -> Sing ( 'TSet a )
  STOperation  :: Sing 'TOperation
  STContract   :: (SingI a, Typeable a)
                => Sing a -> Sing ( 'TContract a )
  STPair       :: (SingI a, SingI b, Typeable a, Typeable b)
                => Sing a -> Sing b -> Sing ('TPair a b)
  STOr         :: (SingI a, SingI b, Typeable a, Typeable b)
                => Sing a -> Sing b -> Sing ('TOr a b)
  STLambda     :: (SingI a, SingI b, Typeable a, Typeable b)
                => Sing a -> Sing b -> Sing ('TLambda a b)
  STMap        :: (SingI a, SingI b, Typeable a, Typeable b)
                => Sing a -> Sing b -> Sing ('TMap a b)
  STBigMap    :: (SingI a, SingI b, Typeable a, Typeable b)
                => Sing a -> Sing b -> Sing ('TBigMap a b)
  STAnnotated :: (SingI a, Typeable a, KnownSymbol n)
                => Sing a -> Proxy n -> Sing ('TFAnnotated ('TFieldAnnS n) a)

-- | This classes fetchs the typeclass dictionary
-- for `Typeable (En a)`.
class GetDict (a :: T) where
  getDict :: Proxy a -> Dict (Typeable (EraseNotes a))

instance (SingI a, Typeable a) => GetDict (a :: T) where
  getDict _ = case sing @a of
    STAnnotated (_ :: Sing p) _ -> getDict (Proxy @p)
    STc _ -> Dict
    STKey -> Dict
    STUnit -> Dict
    STSignature -> Dict
    STOption (_ :: Sing p) -> case getDict (Proxy @p) of
      Dict -> Dict
    STList (_ :: Sing p) -> case getDict (Proxy @p) of
      Dict -> Dict
    STSet _ -> Dict
    STOperation -> Dict
    STContract (_ :: Sing p) -> case getDict (Proxy @p) of
      Dict -> Dict
    STPair (_ :: Sing p) (_ :: Sing q) -> case getDict (Proxy @p) of
      Dict -> case getDict (Proxy @q) of
        Dict -> Dict
    STOr (_ :: Sing p) (_ :: Sing q) -> case getDict (Proxy @p) of
      Dict -> case getDict (Proxy @q) of
        Dict -> Dict
    STLambda (_ :: Sing p) (_ :: Sing q) -> case getDict (Proxy @p) of
      Dict -> case getDict (Proxy @q) of
        Dict -> Dict
    STMap _ (_ :: Sing p) -> case getDict (Proxy @p) of
      Dict -> Dict
    STBigMap _ (_ :: Sing p) -> case getDict (Proxy @p) of
      Dict -> Dict

---------------------------------------------
-- Singleton-related instances for CT
---------------------------------------------

-- | Version of 'SomeSing' with 'Typeable' constraint,
-- specialized for use with 'CT' kind.
data SomeSingCT where
  SomeSingCT :: forall (a :: CT). (SingI a, Typeable a) => Sing a -> SomeSingCT

-- | Version of 'withSomeSing' with 'Typeable' constraint
-- provided to processing function.
--
-- Required for not to erase this useful constraint when doing
-- conversion from value of type 'CT' to its singleton representation.
withSomeSingCT
  :: CT -> (forall (a :: CT). (SingI a, Typeable a) => Sing a -> r) -> r
withSomeSingCT ct f = (\(SomeSingCT s) -> f s) (toSingCT ct)

fromSingCT :: Sing (a :: CT) -> CT
fromSingCT SCInt = CInt
fromSingCT SCNat = CNat
fromSingCT SCString = CString
fromSingCT SCBytes = CBytes
fromSingCT SCMutez = CMutez
fromSingCT SCBool = CBool
fromSingCT SCKeyHash = CKeyHash
fromSingCT SCTimestamp = CTimestamp
fromSingCT SCAddress = CAddress

-- | Version of 'toSing' which creates 'SomeSingCT'.
toSingCT :: CT -> SomeSingCT
toSingCT CInt = SomeSingCT SCInt
toSingCT CNat = SomeSingCT SCNat
toSingCT CString = SomeSingCT SCString
toSingCT CBytes = SomeSingCT SCBytes
toSingCT CMutez = SomeSingCT SCMutez
toSingCT CBool = SomeSingCT SCBool
toSingCT CKeyHash = SomeSingCT SCKeyHash
toSingCT CTimestamp = SomeSingCT SCTimestamp
toSingCT CAddress = SomeSingCT SCAddress

instance SingKind CT where
  type Demote CT = CT
  fromSing  = fromSingCT
  toSing t = case toSingCT t of SomeSingCT s -> SomeSing s

instance SingI  'CInt where
  sing = SCInt
instance SingI  'CNat where
  sing = SCNat
instance SingI  'CString where
  sing = SCString
instance SingI  'CBytes where
  sing = SCBytes
instance SingI  'CMutez where
  sing = SCMutez
instance SingI  'CBool where
  sing = SCBool
instance SingI  'CKeyHash where
  sing = SCKeyHash
instance SingI  'CTimestamp where
  sing = SCTimestamp
instance SingI  'CAddress where
  sing = SCAddress

---------------------------------------------
-- Singleton-related helpers for T
--------------------------------------------

-- | Version of 'SomeSing' with 'Typeable' constraint,
-- specialized for use with 'T' kind.
data SomeSingT where
  SomeSingT :: forall (a :: T). (Typeable a, SingI a)
            => Sing a -> SomeSingT

-- | Version of 'withSomeSing' with 'Typeable' constraint
-- provided to processing function.
--
-- Required for not to erase these useful constraints when doing
-- conversion from value of type 'T' to its singleton representation.
withSomeSingT
  :: T
  -> (forall (a :: T). (Typeable a, SingI a) => Sing a -> r)
  -> r
withSomeSingT t f = (\(SomeSingT s) -> f s) (toSingT t)

-- | Version of 'fromSing' specialized for use with
-- @data instance Sing :: T -> Type@ which requires 'Typeable'
-- constraint for some of its constructors
fromSingT :: Sing (a :: T) -> T
fromSingT (STc t) = Tc (fromSingCT t)
fromSingT STKey = TKey
fromSingT STUnit = TUnit
fromSingT STSignature = TSignature
fromSingT (STOption t) = TOption (fromSingT t)
fromSingT (STList t) = TList (fromSingT t)
fromSingT (STSet t) = TSet (fromSingCT t)
fromSingT STOperation = TOperation
fromSingT (STContract t) = TContract (fromSingT t)
fromSingT (STPair a b) = TPair (fromSingT a) (fromSingT b)
fromSingT (STOr a b) = TOr (fromSingT a) (fromSingT b)
fromSingT (STLambda a b) = TLambda (fromSingT a) (fromSingT b)
fromSingT (STMap a b) = TMap (fromSingCT a) (fromSingT b)
fromSingT (STBigMap a b) = TBigMap (fromSingCT a) (fromSingT b)
fromSingT (STAnnotated b p) = TFAnnotated (TFieldAnn $ symbolValT p) (fromSingT b)

-- | Version of 'toSing' which creates 'SomeSingT'.
toSingT :: T -> SomeSingT
toSingT (Tc ct) = withSomeSingCT ct $ \ctSing -> SomeSingT $ STc ctSing
toSingT TKey = SomeSingT STKey
toSingT TUnit = SomeSingT STUnit
toSingT TSignature = SomeSingT STSignature
toSingT (TOption t) = withSomeSingT t $ \tSing -> SomeSingT $ STOption tSing
toSingT (TList t) = withSomeSingT t $ \tSing -> SomeSingT $ STList tSing
toSingT (TSet ct) = withSomeSingCT ct $ \ctSing -> SomeSingT $ STSet ctSing
toSingT TOperation = SomeSingT STOperation
toSingT (TContract t) =
  withSomeSingT t $ \tSing -> SomeSingT $ STContract tSing
toSingT (TPair l r) =
  withSomeSingT l $ \lSing ->
  withSomeSingT r $ \rSing ->
    SomeSingT $ STPair lSing rSing
toSingT (TOr l r) =
  withSomeSingT l $ \lSing ->
  withSomeSingT r $ \rSing ->
    SomeSingT $ STOr lSing rSing
toSingT (TLambda l r) =
  withSomeSingT l $ \lSing ->
  withSomeSingT r $ \rSing ->
    SomeSingT $ STLambda lSing rSing
toSingT (TMap l r) =
  withSomeSingCT l $ \lSing ->
  withSomeSingT r $ \rSing ->
    SomeSingT $ STMap lSing rSing
toSingT (TBigMap l r) =
  withSomeSingCT l $ \lSing ->
  withSomeSingT r $ \rSing ->
    SomeSingT $ STBigMap lSing rSing
toSingT (TFAnnotated (TFieldAnn n) t) =
  withSomeSingT t $ \tSing ->
    case someSymbolVal (toString n) of
      SomeSymbol p ->
        SomeSingT $ STAnnotated tSing p
toSingT (TFAnnotated (TFieldAnnS _) _) = error "Impossible!"

instance SingKind T where
  type Demote T = T
  fromSing  = fromSingT
  toSing t = case toSingT t of SomeSingT s -> SomeSing s

instance (SingI t, Typeable t) => SingI ( 'Tc (t :: CT)) where
  sing = STc sing
instance SingI  'TKey where
  sing = STKey
instance SingI  'TUnit where
  sing = STUnit
instance SingI  'TSignature where
  sing = STSignature
instance (SingI a, Typeable a) => SingI ( 'TOption (a :: T)) where
  sing = STOption sing
instance (SingI a, Typeable a) => SingI ( 'TList (a :: T)) where
  sing = STList sing
instance (SingI a, Typeable a) => SingI ( 'TSet (a :: CT)) where
  sing = STSet sing
instance SingI 'TOperation where
  sing = STOperation
instance (SingI a, Typeable a) =>
          SingI ( 'TContract (a :: T)) where
  sing = STContract sing
instance (SingI a, Typeable a, Typeable b, SingI b) =>
          SingI ( 'TPair a b) where
  sing = STPair sing sing
instance (SingI a, Typeable a, Typeable b, SingI b) =>
          SingI ( 'TOr a b) where
  sing = STOr sing sing
instance (SingI a, Typeable a, Typeable b, SingI b) =>
          SingI ( 'TLambda a b) where
  sing = STLambda sing sing
instance (SingI a, Typeable a, Typeable b, SingI b) =>
          SingI ( 'TMap a b) where
  sing = STMap sing sing
instance (SingI a, Typeable a, Typeable b, SingI b) =>
          SingI ( 'TBigMap a b) where
  sing = STBigMap sing sing
instance (SingI a, Typeable a, KnownSymbol n) =>
          SingI ( 'TFAnnotated ('TFieldAnnS n) (a :: T)) where
  sing = STAnnotated sing (Proxy :: Proxy n)
