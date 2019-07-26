{-# LANGUAGE QuantifiedConstraints #-}

-- | Parametrizing all fields of a product type.
module Util.Columnar
  ( Columnar
  , type (-/)
  , ColumnarSelectorKind

  , zipColumnarsM
  , zipColumnars

    -- * Utilities
  , columnarUpdate
  , columnarEmptyUpdate
  ) where

import qualified Data.Kind as Kind
import GHC.Generics ((:*:)(..), (:+:))
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage(..), TypeError)

-- | Type family which can be applied to all fields of a datatype.
-- Useful when several types should share the same skeleton but have slightly different fields.
type family Columnar (f :: ColumnarSelectorKind) (a :: Kind.Type) :: Kind.Type

-- | An alias for 'Columnar'.
type f -/ a = Columnar f a
infix 0 -/

type instance Columnar Identity a = a
type instance Columnar Maybe a = Maybe a

-- | Kind of first parameter passed to 'Columnar', we will call it /selector/.
--
-- This kind is chosen because 'Columnar' application is semantically close
-- to wrappers like 'Identity' or 'Maybe'.
type ColumnarSelectorKind = Kind.Type -> Kind.Type

-- | Zip corresponding fields of two datatypes with the same structure.
--
-- Applicable to product types only.
-- For fields which are 'Columnar's the given zipping function is applied,
-- other fields are traversed recursively.
--
-- NB: one of use cases is to merge datatype with itself to perform a traversal
-- over it.
-- Also, you can construct values with as demostrated by 'columnarEmptyUpdate'.
zipColumnars
  :: ZipColumnars ct f g h
  => (forall a. Proxy a -> Columnar f a -> Columnar g a -> Columnar h a)
  -> ct f
  -> ct g
  -> ct h
zipColumnars zipper a b =
  runIdentity $ zipColumnarsM (Identity ... zipper) a b

-- | Version of 'zipColumnars' which allow supplied zipping function to have
-- side effects.
zipColumnarsM
  :: forall ct m f g h.
     (Monad m, ZipColumnars ct f g h)
  => (forall a. Proxy a -> Columnar f a -> Columnar g a -> m (Columnar h a))
  -> ct f
  -> ct g
  -> m (ct h)
zipColumnarsM zipper ctf ctg =
  fmap G.to $
  gZipColumnarsM
    (Proxy @'(f, g, h)) (Proxy @(G.Rep (ct DummyColumnar)))
    zipper (G.from ctf) (G.from ctg)

-- | Helps detecting 'Columnar' fields for sure.
data DummyColumnar :: Kind.Type -> Kind.Type

-- | If we find this as datatype field then it is indeed 'Columnar'
-- because 'Dummy' is not exported outside of the module.
data Dummy a

type instance Columnar DummyColumnar a = Dummy a

-- | Constraint for fields zipping.
type ZipColumnars ct f g h =
  ( Generic (ct f), Generic (ct g), Generic (ct h)
  , GZipColumnars f g h
                  (G.Rep (ct DummyColumnar))
                  (G.Rep (ct f)) (G.Rep (ct g)) (G.Rep (ct h))
  )

-- | Generic traversal for zipping fields.
--
-- First three arguments stand for columnar selector for first
-- and second zipped datatypes and the resulting datatype.
-- Next arguments are generic representations of those datatypes with
-- columnar selector set to 'DummyColumnar', @f@, @g@ and @h@
-- respectively.
class GZipColumnars (f :: Kind.Type -> Kind.Type)
                    (g :: Kind.Type -> Kind.Type)
                    (h :: Kind.Type -> Kind.Type)
                    (xd :: Kind.Type -> Kind.Type)
                    (xf :: Kind.Type -> Kind.Type)
                    (xg :: Kind.Type -> Kind.Type)
                    (xh :: Kind.Type -> Kind.Type)
                      where
  gZipColumnarsM
    :: Applicative m
    => Proxy '(f, g, h)
    -> Proxy xd
    -> (forall a. Proxy a -> Columnar f a -> Columnar g a -> m (Columnar h a))
    -> xf p
    -> xg p
    -> m (xh p)

instance GZipColumnars f g h xd xf xg xh =>
         GZipColumnars f g h (G.M1 t i xd) (G.M1 t i xf) (G.M1 t i xg) (G.M1 t i xh) where
  gZipColumnarsM p _ zipper ~(G.M1 f) ~(G.M1 g) =
    G.M1 <$> gZipColumnarsM p (Proxy @xd) zipper f g

instance
    TypeError ('Text "Cannot zip fields of void-like type") =>
    GZipColumnars f g h G.V1 xf xg xh where
  gZipColumnarsM _ _ _ = error "impossible"

instance
    TypeError ('Text "Cannot zip fields of sum type") =>
    GZipColumnars f g h (a :+: y) xf xg xh where
  gZipColumnarsM _ _ _ = error "impossible"

instance GZipColumnars f g h G.U1 G.U1 G.U1 G.U1 where
  gZipColumnarsM _ _ _ ~G.U1 ~G.U1 = pure G.U1

instance (GZipColumnars f g h xd xf xg xh, GZipColumnars f g h yd yf yg yh) =>
         GZipColumnars f g h (xd :*: yd) (xf :*: yf) (xg :*: yg) (xh :*: yh) where
  gZipColumnarsM p _ zipper ~(xf :*: yf) ~(xg :*: yg) =
    (:*:) <$> gZipColumnarsM p (Proxy @xd) zipper xf xg
          <*> gZipColumnarsM p (Proxy @yd) zipper yf yg

-- Reached field is columnar
instance
    {-# OVERLAPPING #-}
    (af ~ Columnar f a, ag ~ Columnar g a, ah ~ Columnar h a) =>
    GZipColumnars f g h (G.Rec0 (Dummy a)) (G.Rec0 af) (G.Rec0 ag) (G.Rec0 ah) where
  gZipColumnarsM _ _ zipper ~(G.K1 af) ~(G.K1 ag) = G.K1 <$> zipper (Proxy @a) af ag

-- Reached field should be traversed recursively
instance
    {-# OVERLAPPABLE #-}
    ( Generic af, Generic ag, Generic ah
    , GZipColumnars f g h (G.Rep ad) (G.Rep af) (G.Rep ag) (G.Rep ah)
    ) =>
    GZipColumnars f g h (G.Rec0 ad) (G.Rec0 af) (G.Rec0 ag) (G.Rec0 ah) where
  gZipColumnarsM _ _ zipper ~(G.K1 af) ~(G.K1 ag) =
    G.K1 . G.to <$>
    gZipColumnarsM
      (Proxy @'(f, g, h)) (Proxy @(G.Rep ad))
      zipper (G.from af) (G.from ag)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Replace those fields in the first datatype with are set in the second
-- datatype.
columnarUpdate
  :: ZipColumnars ct Identity Maybe Identity
  => ct Identity -> ct Maybe -> ct Identity
columnarUpdate = zipColumnars (\_ -> fromMaybe)

-- | Construct a datatype setting all its fields to 'Nothing'.
columnarEmptyUpdate
  :: ZipColumnars ct Identity Identity Maybe
  => ct Maybe
columnarEmptyUpdate =
  zipColumnars @_ @Identity @Identity
    (\_ _ _ -> Nothing)
    (error "shouldn't be touched")
    (error "shouldn't be touched")
