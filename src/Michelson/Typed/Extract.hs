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
fromUType (Un.Type wholeT _) = conv wholeT
  where
    conv (Un.T_comparable ct) = T_c ct
    conv Un.T_key = T_key
    conv Un.T_unit = T_unit
    conv Un.T_signature = T_signature
    conv (Un.T_option _ t) = T_option (fromUType t)
    conv (Un.T_list t) = T_list (fromUType t)
    conv (Un.T_set (Un.Comparable ct _)) = T_set ct
    conv Un.T_operation = T_operation
    conv (Un.T_contract t) = T_contract (fromUType t)
    conv (Un.T_pair _ _ lT rT) = T_pair (fromUType lT) (fromUType rT)
    conv (Un.T_or _ _ lT rT) = T_or (fromUType lT) (fromUType rT)
    conv (Un.T_lambda lT rT) = T_lambda (fromUType lT) (fromUType rT)
    conv (Un.T_map (Un.Comparable key _) val) = T_map key (fromUType val)
    conv (Un.T_big_map (Un.Comparable key _) val) = T_big_map key (fromUType val)


mkUType :: Sing x -> Notes x -> Un.Type
mkUType sing notes = case (sing, notes) of
  (ST_c ct, N (NT_c tn))              -> mt (Un.T_comparable (fromSingCT ct)) tn
  (ST_c ct, NStar)                    -> mt (Un.T_comparable (fromSingCT ct)) na
  (ST_key, N (NT_key tn))             -> mt Un.T_key tn
  (ST_key, NStar)                     -> mt Un.T_key na
  (ST_unit, N (NT_unit tn))           -> mt Un.T_unit tn
  (ST_unit, NStar)                    -> mt Un.T_unit na
  (ST_signature, N (NT_signature tn)) -> mt Un.T_signature tn
  (ST_signature,NStar)                -> mt Un.T_signature na
  (ST_option t,N (NT_option tn fn n)) -> mt (Un.T_option fn (mkUType t n)) tn
  ((ST_option t), NStar)              -> mt (Un.T_option na (mkUType t NStar)) na
  (ST_list t, N (NT_list tn n))       -> mt (Un.T_list (mkUType t n)) tn
  (ST_list t, NStar)                  -> mt (Un.T_list (mkUType t NStar)) na
  (ST_set ct, N (NT_set tn n))        -> mt (Un.T_set $ mkComp ct n) tn
  (ST_set ct, NStar)                  -> mt (Un.T_set $ mkComp ct na) na
  (ST_operation, N (NT_operation tn)) -> mt Un.T_operation tn
  (ST_operation, NStar)               -> mt Un.T_operation na
  (ST_contract t, N (NT_contract tn n)) ->
    mt (Un.T_contract (mkUType t n)) tn
  (ST_contract t, NStar)              -> mt (Un.T_contract (mkUType t NStar)) na
  (ST_pair tl tr, N (NT_pair tn fl fr nl nr)) ->
    mt (Un.T_pair fl fr (mkUType tl nl) (mkUType tr nr)) tn
  (ST_pair tl tr, NStar) ->
    mt (Un.T_pair na na (mkUType tl NStar) (mkUType tr NStar)) na
  (ST_or tl tr, N (NT_or tn fl fr nl nr)) ->
    mt (Un.T_or fl fr (mkUType tl nl) (mkUType tr nr)) tn
  (ST_or tl tr, NStar) ->
    mt (Un.T_or na na (mkUType tl NStar) (mkUType tr NStar)) na
  (ST_lambda p q, N (NT_lambda tn np nq)) ->
    mt (Un.T_lambda (mkUType p np) (mkUType q nq)) tn
  (ST_lambda p q, NStar) ->
    mt (Un.T_lambda (mkUType p NStar) (mkUType q NStar)) na
  (ST_map k v, N (NT_map tn nk nv)) ->
    mt (Un.T_map (mkComp k nk) (mkUType v nv)) tn
  (ST_map k v, NStar) ->
    mt (Un.T_map (mkComp k na) (mkUType v NStar)) na
  (ST_big_map k v, N (NT_big_map tn nk nv)) ->
    mt (Un.T_big_map (mkComp k nk) (mkUType v nv)) tn
  (ST_big_map k v, NStar) ->
    mt (Un.T_big_map (mkComp k na) (mkUType v NStar)) na
  (ST_custom t, _) -> mkUType t NStar
 where
  mkComp t a = Un.Comparable (fromSingCT t) a
  mt = Un.Type
  na = Un.noAnn

-- | Extracts @Notes t@ type from 'Michelson.Type.Type' and corresponding
-- singleton.
extractNotes :: Un.Type -> Sing t -> Either Text (Notes t)
extractNotes (Un.Type wholeT tn) s = conv wholeT s
  where
    conv :: Un.T -> Sing t -> Either Text (Notes t)
    conv (Un.T_comparable ct) (ST_c cst)
      | fromSingCT cst == ct = pure $ mkNotes $ NT_c tn
    conv Un.T_key ST_key = pure $ mkNotes $ NT_key tn
    conv Un.T_unit ST_unit = pure $ mkNotes $ NT_unit tn
    conv Un.T_signature ST_signature = pure $ mkNotes $ NT_signature tn
    conv (Un.T_option fn t) (ST_option st) =
      mkNotes . NT_option tn fn <$> extractNotes t st
    conv (Un.T_list t) (ST_list st) = do
      mkNotes . NT_list tn <$> extractNotes t st
    conv (Un.T_set (Un.Comparable et sn)) (ST_set est)
      | fromSingCT est == et = pure $ mkNotes $ NT_set tn sn
    conv Un.T_operation ST_operation = pure $ mkNotes $ NT_operation tn
    conv (Un.T_contract t) (ST_contract st) =
      mkNotes . NT_contract tn <$> extractNotes t st
    conv (Un.T_pair pf qf pt qt) (ST_pair spt sqt) =
      liftA2 (mkNotes ... NT_pair tn pf qf)
             (extractNotes pt spt)
             (extractNotes qt sqt)
    conv (Un.T_or pf qf pt qt) (ST_or spt sqt) = do
      liftA2 (mkNotes ... NT_or tn pf qf)
             (extractNotes pt spt)
             (extractNotes qt sqt)
    conv (Un.T_lambda pt qt) (ST_lambda spt sqt) = do
      liftA2 (mkNotes ... NT_lambda tn)
             (extractNotes pt spt)
             (extractNotes qt sqt)
    conv (Un.T_map (Un.Comparable kt kn) vt) (ST_map kst svt)
       | fromSingCT kst == kt = mkNotes . NT_map tn kn  <$> extractNotes vt svt
    conv (Un.T_big_map (Un.Comparable kt kn) vt) (ST_big_map kst svt)
      | fromSingCT kst == kt =
        mkNotes . NT_big_map tn kn  <$> extractNotes vt svt
    conv a (fromSingT -> b) =
      Left $ "failed to construct annotation, provided types do not match: "
                <> show a <> " /= " <> show b

-- | Converts from 'T' to 'Michelson.Type.Type'.
toUType :: T -> Un.Type
toUType t = Un.Type (convert t) Un.noAnn
  where
    convert :: T -> Un.T
    convert (T_c a) = Un.T_comparable a
    convert (T_key) = Un.T_key
    convert (T_unit) = Un.T_unit
    convert (T_signature) = Un.T_signature
    convert (T_option a) = Un.T_option Un.noAnn (toUType a)
    convert (T_list a) = Un.T_list (toUType a)
    convert (T_set a) = Un.T_set $ Un.Comparable a Un.noAnn
    convert (T_operation) = Un.T_operation
    convert (T_contract a) = Un.T_contract (toUType a)
    convert (T_pair a b) =
      Un.T_pair Un.noAnn Un.noAnn (toUType a) (toUType b)
    convert (T_or a b) =
      Un.T_or Un.noAnn Un.noAnn (toUType a) (toUType b)
    convert (T_lambda a b) =
      Un.T_lambda (toUType a) (toUType b)
    convert (T_map a b) =
      Un.T_map (Un.Comparable a Un.noAnn) (toUType b)
    convert (T_big_map a b) =
      Un.T_big_map (Un.Comparable a Un.noAnn) (toUType b)
    convert (T_custom _ t') = convert t'
