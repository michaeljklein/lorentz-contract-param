{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Conversion between 'UStore' in Haskell and Michelson representation.
module Lorentz.UStore.Haskell
  ( UStoreContent
  , UStoreConversible
  , mkUStore
  , ustoreDecompose
  , ustoreDecomposeFull
  ) where

import qualified Data.Kind as Kind
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Singletons (demote)
import Fmt ((+|), (+||), (|+), (||+))
import GHC.Generics ((:*:)(..), (:+:)(..))
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage(..), KnownSymbol, TypeError, symbolVal)

import Lorentz.Constraints
import Lorentz.UStore.Common
import Lorentz.UStore.Types
import Michelson.Interpret.Pack
import Michelson.Interpret.Unpack
import Michelson.Text
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Scope
import Util.Columnar

-- | 'UStore' content represented as key-value pairs.
type UStoreContent = [(ByteString, ByteString)]

-- | Make 'UStore' from separate @big_map@s and fields.
mkUStore
  :: forall template.
     ( Generic (template UStoreTypesOnly)
     , Generic (template UStoreValue)
     , UStoreConversible template
     )
  => template UStoreValue -> UStore template
mkUStore = UStore . BigMap . mkUStoreRec @(template UStoreTypesOnly)

-- | Decompose 'UStore' into separate @big_map@s and fields.
--
-- Since this function needs to @UNPACK@ content of @UStore@ to actual
-- keys and values, you have to provide 'UnpackEnv'.
--
-- Along with resulting value, you get a list of @UStore@ entries which
-- were not recognized as belonging to any submap or field according to
-- @UStore@'s template - this should be empty unless @UStore@ invariants
-- were violated.
ustoreDecompose
  :: forall template.
     ( Generic (template UStoreTypesOnly)
     , Generic (template UStoreValue)
     , UStoreConversible template
     )
  => UnpackEnv -> UStore template -> Either Text (UStoreContent, template UStoreValue)
ustoreDecompose ue =
  storeDecomposeRec @(template UStoreTypesOnly) ue .
  Map.toList . unBigMap . unUStore

-- | Like 'ustoreDecompose', but requires all entries from @UStore@ to be
-- recognized.
ustoreDecomposeFull
  :: forall template.
     ( Generic (template UStoreTypesOnly)
     , Generic (template UStoreValue)
     , UStoreConversible template
     )
  => UnpackEnv -> UStore template -> Either Text (template UStoreValue)
ustoreDecomposeFull ue ustore = do
  (remained, res) <- ustoreDecompose ue ustore
  unless (null remained) $
    Left $ "Unrecognized entries in UStore: " +|| remained ||+ ""
  return res

-- | Recursive template traversal for 'mkUStore'.
mkUStoreRec
  :: forall templateTypes templateValue.
     ( Generic templateTypes, Generic templateValue
     , UStoreConversibleRec templateTypes templateValue
     )
  => templateValue -> Map ByteString ByteString
mkUStoreRec = gUstoreToVal @(G.Rep templateTypes) . G.from

-- | Recursive template traversal for 'ustoreDecompose'.
storeDecomposeRec
  :: forall templateTypes templateValue.
     ( Generic templateTypes, Generic templateValue
     , UStoreConversibleRec templateTypes templateValue
     )
  => UnpackEnv -> UStoreContent -> Either Text (UStoreContent, templateValue)
storeDecomposeRec = fmap (second G.to) ... gUstoreFromVal @(G.Rep templateTypes)

-- | Given template can be converted to 'UStore' value.
class UStoreConversibleRec (template UStoreTypesOnly)
                           (template UStoreValue) =>
      UStoreConversible template
instance UStoreConversibleRec (template UStoreTypesOnly)
                              (template UStoreValue) =>
      UStoreConversible template

type UStoreConversibleRec templateTypes templateValue =
  GUStoreConversible (G.Rep templateTypes) (G.Rep templateValue)

-- | Generic traversal for 'mkUStore' and 'ustoreDecompose'.
--
-- It is parametrized by UStore template with columnar selector set to
-- 1. 'UStoreTypesOnly' for unambiguous type-level analysis;
-- 2. 'UStoreValue' which is type of the converted value itself.
class GUStoreConversible (templateTyped :: Kind.Type -> Kind.Type)
                         (templateValue :: Kind.Type -> Kind.Type) where
  -- | Convert generic value to internal 'UStore' representation.
  gUstoreToVal :: templateValue p -> Map ByteString ByteString

  -- | Parse internal 'UStore' representation into generic Haskell value of
  -- 'UStore', also returning unparsed entries.
  gUstoreFromVal
    :: UnpackEnv
    -> UStoreContent
    -> Either Text (UStoreContent, templateValue p)

instance GUStoreConversible xt xi => GUStoreConversible (G.D1 i xt) (G.D1 i xi) where
  gUstoreToVal = gUstoreToVal @xt . G.unM1
  gUstoreFromVal = fmap (second G.M1) ... gUstoreFromVal @xt

instance GUStoreConversible xt xi => GUStoreConversible (G.C1 i xt) (G.C1 i xi) where
  gUstoreToVal = gUstoreToVal @xt . G.unM1
  gUstoreFromVal = fmap (second G.M1) ... gUstoreFromVal @xt

instance TypeError ('Text "Unexpected sum type in UStore template") =>
         GUStoreConversible (x :+: y) xi where
  gUstoreToVal = error "impossible"
  gUstoreFromVal = error "impossible"

instance TypeError ('Text "UStore template should have one constructor") =>
         GUStoreConversible G.V1 xi where
  gUstoreToVal = error "impossible"
  gUstoreFromVal = error "impossible"

instance (GUStoreConversible xt xi, GUStoreConversible yt yi) =>
         GUStoreConversible (xt :*: yt) (xi :*: yi) where
  gUstoreToVal (x :*: y) = gUstoreToVal @xt x <> gUstoreToVal @yt y
  gUstoreFromVal unpackEnv entries = do
    (entries', res1) <- gUstoreFromVal @xt unpackEnv entries
    (entries'', res2) <- gUstoreFromVal @yt unpackEnv entries'
    return (entries'', res1 :*: res2)

instance GUStoreConversible G.U1 G.U1 where
  gUstoreToVal G.U1 = mempty
  gUstoreFromVal _ entries = pure (entries, G.U1)

-- | Case of nested template.
instance {-# OVERLAPPABLE #-}
         ( Generic at, Generic ai, UStoreConversibleRec at ai
         , xi ~ (G.S1 i (G.Rec0 ai))
         ) =>
         GUStoreConversible (G.S1 i (G.Rec0 at)) xi where
  gUstoreToVal = mkUStoreRec @at . G.unK1 . G.unM1
  gUstoreFromVal = fmap (second $ G.M1 . G.K1) ... storeDecomposeRec @at

-- | Case of '|~>'.
instance ( Each [IsoValue, KnownValue, NoOperation, NoBigMap] [k, v]
         , KnownSymbol fieldName, Ord k
         , xi ~ G.S1 ('G.MetaSel ('Just fieldName) _1 _2 _3)
                     (G.Rec0 (Map k v))
         ) =>
         GUStoreConversible (G.S1 ('G.MetaSel ('Just fieldName) _1 _2 _3)
                                  (G.Rec0 (k |~> v)))
                            xi where
  gUstoreToVal (G.M1 (G.K1 m)) =
    forbiddenOp @(ToT k) $ forbiddenBigMap @(ToT k) $
    forbiddenOp @(ToT v) $ forbiddenBigMap @(ToT v) $
      mconcat
        [ one ( packValue' $ toVal (fieldNameToMText @fieldName, k)
              , packValue' $ toVal v
              )
        | (k, v) <- Map.toList m
        ]

  gUstoreFromVal unpackEnv allEntries = do
    (unrecognized, res) <- foldM parseEntry (mempty, mempty) allEntries
    return (unrecognized, G.M1 $ G.K1 res)
    where
    parseEntry
      :: (UStoreContent, Map k v)
      -> (ByteString, ByteString)
      -> Either Text (UStoreContent, Map k v)
    parseEntry (entries, !acc) entry@(key, val) =
      forbiddenOp @(ToT k) $ forbiddenBigMap @(ToT k) $
      forbiddenOp @(ToT v) $ forbiddenBigMap @(ToT v) $

        case unpackValue' unpackEnv key of
          Left _ -> Right (entry : entries, acc)
          Right (fromVal -> (name :: MText, keyValue :: k))
            | toText name /= toText (symbolVal $ Proxy @fieldName) ->
                Right (entry : entries, acc)
            | otherwise ->
              case unpackValue' unpackEnv val of
                Left err ->
                  Left $ "Failed to parse UStore value for " +|
                        demote @(ToT k) |+ " |~> " +| demote @(ToT v) |+
                        ": " +| err |+ ""
                Right (fromVal -> valValue) ->
                  Right (entries, Map.insert keyValue valValue acc)

-- | Case of 'UStoreField'.
instance ( Each [IsoValue, KnownValue, NoOperation, NoBigMap] '[v]
         , KnownSymbol fieldName
         , xi ~ G.S1 ('G.MetaSel ('Just fieldName) _1 _2 _3) (G.Rec0 v)
         ) =>
         GUStoreConversible (G.S1 ('G.MetaSel ('Just fieldName) _1 _2 _3)
                                  (G.Rec0 (UStoreField v)))
                            xi where
  gUstoreToVal (G.M1 (G.K1 val)) =
    forbiddenOp @(ToT v) $ forbiddenBigMap @(ToT v) $
      one ( packValue' $ toVal (fieldNameToMText @fieldName)
          , packValue' $ toVal val
          )

  gUstoreFromVal unpackEnv entries =
    forbiddenOp @(ToT v) $ forbiddenBigMap @(ToT v) $
      let key = packValue' $ toVal (fieldNameToMText @fieldName)
      in case L.partition ((== key) . fst) entries of
          ([], _) ->
            Left $ "Failed to find field in UStore: " +|
                  fieldNameToMText @fieldName |+ ""
          ([(_, val)], otherEntries) ->
            case unpackValue' unpackEnv val of
              Left err ->
                Left $ "Failed to parse UStore value for field " +|
                      demote @(ToT v) |+ ": " +| err |+ ""
              Right (fromVal -> valValue) ->
                Right (otherEntries, G.M1 $ G.K1 valValue)
          (_ : _ : _, _) ->
            error "UStore content contained multiple entries with the same key"


-- Examples
----------------------------------------------------------------------------

data MyStoreTemplate f = MyStoreTemplate
  { ints :: f -/ Integer |~> ()
  , flag :: f -/ UStoreField Bool
  }
  deriving stock (Generic)

data MyStoreTemplateBig f = MyStoreTemplateBig
  { templ :: MyStoreTemplate f
  , bytes :: f -/ ByteString |~> ByteString
  }
  deriving stock (Generic)

_storeSample :: UStore MyStoreTemplate
_storeSample = mkUStore
  MyStoreTemplate
  { ints = one (1, ())
  , flag = False
  }

_storeSampleBig :: UStore MyStoreTemplateBig
_storeSampleBig = mkUStore $
  MyStoreTemplateBig
    MyStoreTemplate
      { ints = one (1, ())
      , flag = False
      }
    (one ("a", "b"))
