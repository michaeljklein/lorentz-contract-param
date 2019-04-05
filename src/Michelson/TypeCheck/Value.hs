module Michelson.TypeCheck.Value
    ( typeCheckValImpl
    , typeCheckCVal
    ) where

import Control.Monad.Except (liftEither, throwError)
import Data.Default (def)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Typeable ((:~:)(..))
import Prelude hiding (EQ, GT, LT)

import Michelson.TypeCheck.Helpers
import Michelson.TypeCheck.Types
import Michelson.Typed
  (CT(..), ConversibleExt, Instr(..), InstrExtT, Notes(..), Notes'(..), Sing(..), T(..), converge,
  mkNotes, withSomeSingCT, withSomeSingT)
import Michelson.Typed.Value (CVal(..), Val(..))
import qualified Michelson.Untyped as Un
import Tezos.Address (parseAddress)
import Tezos.Core (mkMutez, parseTimestamp, timestampFromSeconds)
import Tezos.Crypto (parseKeyHash, parsePublicKey, parseSignature)

typeCheckCVal :: Un.Value op -> CT -> Maybe SomeValC
typeCheckCVal (Un.ValueInt i) CInt = pure $ CvInt i :--: SCInt
typeCheckCVal (Un.ValueInt i) CNat
  | i >= 0 = pure $ CvNat (fromInteger i) :--: SCNat
typeCheckCVal (Un.ValueInt (mkMutez . fromInteger -> Just mtz)) CMutez =
  pure $ CvMutez mtz :--: SCMutez
typeCheckCVal (Un.ValueString s) CString =
  pure $ CvString s :--: SCString
typeCheckCVal (Un.ValueString (parseAddress -> Right s)) CAddress =
  pure $ CvAddress s :--: SCAddress
typeCheckCVal (Un.ValueString (parseKeyHash -> Right s)) CKeyHash =
  pure $ CvKeyHash s :--: SCKeyHash
typeCheckCVal (Un.ValueString (parseTimestamp -> Just t)) CTimestamp =
  pure $ CvTimestamp t :--: SCTimestamp
typeCheckCVal (Un.ValueInt i) CTimestamp =
  pure $ CvTimestamp (timestampFromSeconds i) :--: SCTimestamp
typeCheckCVal (Un.ValueBytes (Un.InternalByteString s)) CBytes =
  pure $ CvBytes s :--: SCBytes
typeCheckCVal Un.ValueTrue CBool = pure $ CvBool True :--: SCBool
typeCheckCVal Un.ValueFalse CBool = pure $ CvBool False :--: SCBool
typeCheckCVal _ _ = Nothing

typeCheckCVals
  :: forall t op . Typeable t
  => [Un.Value op]
  -> CT
  -> Either (Un.Value op, Text) [CVal t]
typeCheckCVals mvs t = traverse check mvs
  where
    check mv = do
      v :--: (_ :: Sing t') <-
        maybe (Left (mv, "failed to typecheck cval")) pure $ typeCheckCVal mv t
      Refl <- eqT' @t @t' `onLeft` (,) mv
      pure v

-- | Function @typeCheckValImpl@ converts a single Michelson value
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- As a third argument, @typeCheckValImpl@ accepts expected type of value.
--
-- Type checking algorithm pattern-matches on parse value representation,
-- expected type @t@ and constructs @Val t@ value.
--
-- If there was no match on a given pair of value and expected type,
-- that is interpreted as input of wrong type and type check finishes with
-- error.
typeCheckValImpl
  :: (Show InstrExtT, ConversibleExt, Eq Un.ExpandedInstrExtU)
  => TcInstrHandler
  -> Un.UntypedValue
  -> T
  -> TypeCheckT SomeVal
typeCheckValImpl _ mv t@(Tc ct) =
  maybe (throwError $ TCFailedOnValue mv t "")
        (\(v :--: cst) -> pure $ VC v :::: (STc cst, NStar))
        (typeCheckCVal mv ct)
typeCheckValImpl _ (Un.ValueString (parsePublicKey -> Right s)) TKey =
  pure $ VKey s :::: (STKey, NStar)

typeCheckValImpl _ (Un.ValueString (parseSignature -> Right s)) TSignature =
  pure $ VSignature s :::: (STSignature, NStar)

typeCheckValImpl _ (Un.ValueString (parseAddress -> Right s)) (TContract pt) =
  withSomeSingT pt $ \p ->
    pure $ VContract s :::: (STContract p, NStar)
typeCheckValImpl _ Un.ValueUnit TUnit = pure $ VUnit :::: (STUnit, NStar)
typeCheckValImpl tcDo (Un.ValuePair ml mr) (TPair lt rt) = do
  l :::: (lst, ln) <- typeCheckValImpl tcDo ml lt
  r :::: (rst, rn) <- typeCheckValImpl tcDo mr rt
  let ns = mkNotes $ NTPair def def def ln rn
  pure $ VPair (l, r) :::: (STPair lst rst, ns)
typeCheckValImpl tcDo (Un.ValueLeft ml) (TOr lt rt) = do
  l :::: (lst, ln) <- typeCheckValImpl tcDo ml lt
  withSomeSingT rt $ \rst ->
    pure $ VOr (Left l) :::: ( STOr lst rst
                             , mkNotes $ NTOr def def def ln NStar )
typeCheckValImpl tcDo (Un.ValueRight mr) (TOr lt rt) = do
  r :::: (rst, rn) <- typeCheckValImpl tcDo mr rt
  withSomeSingT lt $ \lst ->
    pure $ VOr (Right r) :::: ( STOr lst rst
                              , mkNotes $ NTOr def def def NStar rn )
typeCheckValImpl tcDo (Un.ValueSome mv) (TOption vt) = do
  v :::: (vst, vns) <- typeCheckValImpl tcDo mv vt
  let ns = mkNotes $ NTOption def def vns
  pure $ VOption (Just v) :::: (STOption vst, ns)
typeCheckValImpl _ Un.ValueNone (TOption vt) =
  withSomeSingT vt $ \vst ->
    pure $ VOption Nothing :::: (STOption vst, NStar)

typeCheckValImpl _ Un.ValueNil (TList vt) =
  withSomeSingT vt $ \vst ->
    pure $ VList [] :::: (STList vst, mkNotes $ NTList def NStar)

typeCheckValImpl tcDo (Un.ValueSeq (toList -> mels)) (TList vt) =
  withSomeSingT vt $ \vst -> do
    (els, ns) <- typeCheckValsImpl tcDo mels vt
    pure $ VList els :::: (STList vst, mkNotes $ NTList def ns)

typeCheckValImpl _ Un.ValueNil (TSet vt) =
  withSomeSingCT vt $ \vst ->
    pure $ VSet S.empty :::: (STSet vst, NStar)

typeCheckValImpl _ sq@(Un.ValueSeq (toList -> mels)) (TSet vt) =
  withSomeSingCT vt $ \vst -> do
    els <- liftEither $ typeCheckCVals mels vt
            `onLeft` \(cv, err) -> TCFailedOnValue cv (Tc vt) $
                                      "wrong type of set element: " <> err
    elsS <- liftEither $ S.fromDistinctAscList <$> ensureDistinctAsc els
            `onLeft` TCFailedOnValue sq (Tc vt)
    pure $ VSet elsS :::: (STSet vst, NStar)

typeCheckValImpl _ Un.ValueNil (TMap kt vt) =
  withSomeSingT vt $ \vst ->
  withSomeSingCT kt $ \kst -> do
    let ns = mkNotes $ NTMap def def NStar
    pure $ VMap M.empty :::: (STMap kst vst, ns)

typeCheckValImpl tcDo sq@(Un.ValueMap (toList -> mels)) (TMap kt vt) =
  withSomeSingT vt $ \vst ->
  withSomeSingCT kt $ \kst -> do
    ks <- liftEither $  typeCheckCVals (map (\(Un.Elt k _) -> k) mels) kt
            `onLeft` \(cv, err) -> TCFailedOnValue cv (Tc kt) $
                                      "wrong type of map key: " <> err
    (vals, vns) <- typeCheckValsImpl tcDo (map (\(Un.Elt _ v) -> v) mels) vt
    let ns = mkNotes $ NTMap def def vns
    ksS <- liftEither $ ensureDistinctAsc ks
            `onLeft` TCFailedOnValue sq (Tc kt)
    pure $ VMap (M.fromDistinctAscList $ zip ksS vals) :::: (STMap kst vst, ns)

typeCheckValImpl tcDo v t@(TLambda mi mo) = do
  mp <- case v of
    Un.ValueNil -> pure []
    Un.ValueLambda mp -> pure $ toList mp
    _ -> throwError $ TCFailedOnValue v t ""

  withSomeSingT mi $ \(it :: Sing it) ->
    withSomeSingT mo $ \(ot :: Sing ot) ->
      typeCheckImpl tcDo (Un.SeqEx mp) ((it, NStar, def) ::& SNil) >>= \case
        SiFail -> pure $ VLam FAILWITH :::: (STLambda it ot, NStar)
        lam ::: (li, (lo :: HST lo)) -> do
          case eqT' @'[ ot ] @lo of
            Right Refl -> do
              let (_, ons, _) ::& SNil = lo
              let (_, ins, _) ::& SNil = li
              let ns = mkNotes $ NTLambda def ins ons
              pure $ VLam lam :::: (STLambda it ot, ns)
            Left m ->
              throwError $ TCFailedOnValue v t $
                      "wrong output type of lambda's value: " <> m

typeCheckValImpl _ v t = throwError $ TCFailedOnValue v t ""

typeCheckValsImpl
  :: forall t . (Typeable t, Show InstrExtT, ConversibleExt, Eq Un.ExpandedInstrExtU)
  => TcInstrHandler
  -> [Un.UntypedValue]
  -> T
  -> TypeCheckT ([Val Instr t], Notes t)
typeCheckValsImpl tcDo mvs t =
  fmap (first reverse) $ foldM check ([], NStar) mvs
  where
    check (res, ns) mv = do
      v :::: ((_ :: Sing t'), vns) <- typeCheckValImpl tcDo mv t
      Refl <- liftEither $ eqT' @t @t'
                `onLeft` (TCFailedOnValue mv t . ("wrong element type " <>))
      ns' <- liftEither $ converge ns vns `onLeft` TCFailedOnValue mv t
      pure (v : res, ns')
