-- | Conversions between tuples and list-like types.
module Util.TypeTuple
  ( RecFromTuple (..)
  ) where

import Data.Vinyl.Core (Rec (..))
import qualified Data.Kind as Kind

-- | Building a record from tuple.
--
-- It differs from similar typeclass in 'Data.Vinyl.FromTuple' module in that
-- it allows type inference outside-in - knowing desired 'Rec' you know which
-- tuple should be provided - this improves error messages when constructing
-- concrete 'Rec' objects.
class RecFromTuple r where
  type IsoRecTuple r :: Kind.Type
  recFromTuple :: IsoRecTuple r -> r

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[]) where
  type IsoRecTuple (Rec f '[]) = ()
  recFromTuple () = RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a]) where
  type IsoRecTuple (Rec f '[a]) = f a
  recFromTuple a = a :& RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a, b]) where
  type IsoRecTuple (Rec f '[a, b]) = (f a, f b)
  recFromTuple (a, b) = a :& b :& RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a, b, c]) where
  type IsoRecTuple (Rec f '[a, b, c]) = (f a, f b, f c)
  recFromTuple (a, b, c) = a :& b :& c :& RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a, b, c, d]) where
  type IsoRecTuple (Rec f '[a, b, c, d]) = (f a, f b, f c, f d)
  recFromTuple (a, b, c, d) = a :& b :& c :& d :& RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a, b, c, d, e]) where
  type IsoRecTuple (Rec f '[a, b, c, d, e]) = (f a, f b, f c, f d, f e)
  recFromTuple (a, b, c, d, e) = a :& b :& c :& d :& e :& RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a, b, c, d, e, g]) where
  type IsoRecTuple (Rec f '[a, b, c, d, e, g]) = (f a, f b, f c, f d, f e, f g)
  recFromTuple (a, b, c, d, e, g) = a :& b :& c :& d :& e :& g :& RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a, b, c, d, e, g, h]) where
  type IsoRecTuple (Rec f '[a, b, c, d, e, g, h]) = (f a, f b, f c, f d, f e, f g, f h)
  recFromTuple (a, b, c, d, e, g, h) = a :& b :& c :& d :& e :& g :& h :& RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a, b, c, d, e, g, h, i]) where
  type IsoRecTuple (Rec f '[a, b, c, d, e, g, h, i]) = (f a, f b, f c, f d, f e, f g, f h, f i)
  recFromTuple (a, b, c, d, e, g, h, i) = a :& b :& c :& d :& e :& g :& h :& i :& RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a, b, c, d, e, g, h, i, j]) where
  type IsoRecTuple (Rec f '[a, b, c, d, e, g, h, i, j]) = (f a, f b, f c, f d, f e, f g, f h, f i, f j)
  recFromTuple (a, b, c, d, e, g, h, i, j) = a :& b :& c :& d :& e :& g :& h :& i :& j :& RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a, b, c, d, e, g, h, i, j, k]) where
  type IsoRecTuple (Rec f '[a, b, c, d, e, g, h, i, j, k]) = (f a, f b, f c, f d, f e, f g, f h, f i, f j, f k)
  recFromTuple (a, b, c, d, e, g, h, i, j, k) = a :& b :& c :& d :& e :& g :& h :& i :& j :& k :& RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a, b, c, d, e, g, h, i, j, k, l]) where
  type IsoRecTuple (Rec f '[a, b, c, d, e, g, h, i, j, k, l]) = (f a, f b, f c, f d, f e, f g, f h, f i, f j, f k, f l)
  recFromTuple (a, b, c, d, e, g, h, i, j, k, l) = a :& b :& c :& d :& e :& g :& h :& i :& j :& k :& l :& RNil

instance RecFromTuple (Rec (f :: u -> Kind.Type) '[a, b, c, d, e, g, h, i, j, k, l, m]) where
  type IsoRecTuple (Rec f '[a, b, c, d, e, g, h, i, j, k, l, m]) = (f a, f b, f c, f d, f e, f g, f h, f i, f j, f k, f l, f m)
  recFromTuple (a, b, c, d, e, g, h, i, j, k , l, m) = a :& b :& c :& d :& e :& g :& h :& i :& j :& k :& l :& m :& RNil
