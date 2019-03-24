{-# LANGUAGE DataKinds, GADTs #-}

-- Module, containing functions to convert @Michelson.Types.Type@ to
-- @Michelson.Typed.T.T@ Michelson type representation (type stripped off all
-- annotations) and to @Michelson.Typed.Annotation.Notes@ value (which contains
-- field and type annotations for a given Michelson type).
--
-- I.e. @Michelson.Types.Type@ is split to value @t :: T@ and value of type
-- @Notes t@ for which @t@ is a type representation of value @t@.
module Michelson.Typed.Extract
  ( extractNotes
  , fromUType
  , mkUType
  , toUType
  ) where

import Michelson.Typed.Annotation (Notes(..), Notes'(..), mkNotes)
import Michelson.Typed.Sing (Sing(..), fromSingCT, fromSingT)
import Michelson.Typed.T (T(..))
import qualified Michelson.Untyped as Un

-- | Extracts 'T' type from 'Michelson.Untyped.Type'.
fromUType :: Un.Type -> T
fromUType (Un.Type wholeT _) = case wholeT of
  Un.Tc ct                         -> Tc ct
  Un.TKey                          -> TKey
  Un.TUnit                         -> TUnit
  Un.TSignature                    -> TSignature
  Un.TOption _ t                   -> TOption (fromUType t)
  Un.TList t                       -> TList (fromUType t)
  Un.TSet (Un.Comparable ct _)     -> TSet ct
  Un.TOperation                    -> TOperation
  Un.TContract t                   -> TContract (fromUType t)
  Un.TPair _ _ lT rT               -> TPair (fromUType lT) (fromUType rT)
  Un.TOr _ _ lT rT                 -> TOr (fromUType lT) (fromUType rT)
  Un.TLambda lT rT                 -> TLambda (fromUType lT) (fromUType rT)
  Un.TMap (Un.Comparable k _) v    -> TMap k (fromUType v)
  Un.TBigMap (Un.Comparable k _) v -> TBigMap k (fromUType v)


mkUType :: Sing x -> Notes x -> Un.Type
mkUType sing notes = case (sing, notes) of
  (STc ct, N (NTc tn))                -> ut (Un.Tc (fromSingCT ct)) tn
  (STc ct, NStar)                     -> ut (Un.Tc (fromSingCT ct)) na
  (STKey, N (NTKey tn))               -> ut Un.TKey tn
  (STKey, NStar)                      -> ut Un.TKey na
  (STUnit, N (NTUnit tn))             -> ut Un.TUnit tn
  (STUnit, NStar)                     -> ut Un.TUnit na
  (STSignature, N (NTSignature tn))   -> ut Un.TSignature tn
  (STSignature,NStar)                 -> ut Un.TSignature na
  (STOption t,N (NTOption tn fn n))   -> ut (Un.TOption fn (mkUType t n)) tn
  ((STOption t), NStar)               -> ut (Un.TOption na (mkUType t NStar)) na
  (STList t, N (NTList tn n))         -> ut (Un.TList (mkUType t n)) tn
  (STList t, NStar)                   -> ut (Un.TList (mkUType t NStar)) na
  (STSet ct, N (NTSet tn n))          -> ut (Un.TSet $ mkComp ct n) tn
  (STSet ct, NStar)                   -> ut (Un.TSet $ mkComp ct na) na
  (STOperation, N (NTOperation tn))   -> ut Un.TOperation tn
  (STOperation, NStar)                -> ut Un.TOperation na
  (STContract t, N (NTContract tn n)) -> ut (Un.TContract (mkUType t n)) tn
  (STContract t, NStar)               -> ut (Un.TContract (mkUType t NStar)) na
  (STPair tl tr, N (NTPair tn fl fr nl nr)) ->
    ut (Un.TPair fl fr (mkUType tl nl) (mkUType tr nr)) tn
  (STPair tl tr, NStar) ->
    ut (Un.TPair na na (mkUType tl NStar) (mkUType tr NStar)) na
  (STOr tl tr, N (NTOr tn fl fr nl nr)) ->
    ut (Un.TOr fl fr (mkUType tl nl) (mkUType tr nr)) tn
  (STOr tl tr, NStar) ->
    ut (Un.TOr na na (mkUType tl NStar) (mkUType tr NStar)) na
  (STLambda p q, N (NTLambda tn np nq)) ->
    ut (Un.TLambda (mkUType p np) (mkUType q nq)) tn
  (STLambda p q, NStar) ->
    ut (Un.TLambda (mkUType p NStar) (mkUType q NStar)) na
  (STMap k v, N (NTMap tn nk nv)) ->
    ut (Un.TMap (mkComp k nk) (mkUType v nv)) tn
  (STMap k v, NStar) ->
      ut (Un.TMap (mkComp k na) (mkUType v NStar)) na
  (STBigMap k v, N (NTBigMap tn nk nv)) ->
    ut (Un.TBigMap (mkComp k nk) (mkUType v nv)) tn
  (STBigMap k v, NStar) ->
    ut (Un.TBigMap (mkComp k na) (mkUType v NStar)) na
 where
  mkComp t a = Un.Comparable (fromSingCT t) a
  ut = Un.Type
  na = Un.noAnn

-- | Extracts @Notes t@ type from 'Michelson.Type.Type' and corresponding
-- singleton.
extractNotes :: Un.Type -> Sing t -> Either Text (Notes t)
extractNotes (Un.Type wholeT tn) s = case (wholeT, s) of
  (Un.Tc ct, STc cst)
    | fromSingCT cst == ct       -> pure $ mkNotes $ NTc tn
  (Un.TKey, STKey)               -> pure $ mkNotes $ NTKey tn
  (Un.TUnit, STUnit)             -> pure $ mkNotes $ NTUnit tn
  (Un.TSignature, STSignature)   -> pure $ mkNotes $ NTSignature tn
  (Un.TOption f t, STOption p)   -> mkNotes . NTOption tn f <$> extractNotes t p
  (Un.TList t, STList st)        -> mkNotes . NTList tn <$> extractNotes t st
  (Un.TSet (Un.Comparable et sn), STSet est)
    | fromSingCT est == et       -> pure $ mkNotes $ NTSet tn sn
  (Un.TOperation, STOperation)   -> pure $ mkNotes $ NTOperation tn
  (Un.TContract t, STContract p) -> mkNotes . NTContract tn <$> extractNotes t p
  (Un.TPair l r a b, STPair p q) -> liftA2 (mkNotes ... NTPair tn l r)
                                    (extractNotes a p)
                                    (extractNotes b q)
  (Un.TOr l r a b, STOr p q)     -> liftA2 (mkNotes ... NTOr tn l r)
                                    (extractNotes a p)
                                    (extractNotes b q)
  (Un.TLambda a b, STLambda p q) -> liftA2 (mkNotes ... NTLambda tn)
                                    (extractNotes a p)
                                    (extractNotes b q)
  (Un.TMap (Un.Comparable k kn) v, STMap sk sv)
     | fromSingCT sk == k        -> mkNotes . NTMap tn kn <$> extractNotes v sv
  (Un.TBigMap (Un.Comparable k kn) v, STBigMap sk sv)
    | fromSingCT sk == k         -> mkNotes . NTBigMap tn kn <$> extractNotes v sv
  (a, fromSingT -> b)            ->
    Left $ "failed to construct annotation, provided types do not match: "
              <> show a <> " /= " <> show b

-- | Converts from 'T' to 'Michelson.Type.Type'.
toUType :: T -> Un.Type
toUType = (flip Un.Type $ Un.noAnn) . \case
  Tc a        -> Un.Tc a
  TKey        -> Un.TKey
  TUnit       -> Un.TUnit
  TSignature  -> Un.TSignature
  TOption a   -> Un.TOption Un.noAnn (toUType a)
  TList a     -> Un.TList (toUType a)
  TSet a      -> Un.TSet $ Un.Comparable a Un.noAnn
  TOperation  -> Un.TOperation
  TContract a -> Un.TContract (toUType a)
  TPair a b   -> Un.TPair Un.noAnn Un.noAnn (toUType a) (toUType b)
  TOr a b     -> Un.TOr Un.noAnn Un.noAnn (toUType a) (toUType b)
  TLambda a b -> Un.TLambda (toUType a) (toUType b)
  TMap a b    -> Un.TMap (Un.Comparable a Un.noAnn) (toUType b)
  TBigMap a b -> Un.TBigMap (Un.Comparable a Un.noAnn) (toUType b)
