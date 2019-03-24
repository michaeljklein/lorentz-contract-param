module Michelson.TypeCheck.Value
    ( typeCheckValueImpl
    , typeCheckCValue
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
  ((:+>)(..), CT(..), Instr, InstrExtT, Notes(..), Notes'(..), Sing(..), T(..), converge, mkNotes,
  withSomeSingCT, withSomeSingT)
import Michelson.Typed.Value (CValue(..), Value(..))
import qualified Michelson.Untyped as Un
import Tezos.Address (parseAddress)
import Tezos.Core (mkMutez, parseTimestamp, timestampFromSeconds)
import Tezos.Crypto (parseKeyHash, parsePublicKey, parseSignature)

typeCheckCValue :: Un.Value op -> CT -> Maybe SomeValC
typeCheckCValue (Un.ValueInt i) CInt = pure $ CvInt i :--: SCInt
typeCheckCValue (Un.ValueInt i) CNat
  | i >= 0 = pure $ CvNat (fromInteger i) :--: SCNat
typeCheckCValue (Un.ValueInt (mkMutez . fromInteger -> Just mtz)) CMutez =
  pure $ CvMutez mtz :--: SCMutez
typeCheckCValue (Un.ValueString s) CString =
  pure $ CvString s :--: SCString
typeCheckCValue (Un.ValueString (parseAddress -> Right s)) CAddress =
  pure $ CvAddress s :--: SCAddress
typeCheckCValue (Un.ValueString (parseKeyHash -> Right s)) CKeyHash =
  pure $ CvKeyHash s :--: SCKeyHash
typeCheckCValue (Un.ValueString (parseTimestamp -> Just t)) CTimestamp =
  pure $ CvTimestamp t :--: SCTimestamp
typeCheckCValue (Un.ValueInt i) CTimestamp =
  pure $ CvTimestamp (timestampFromSeconds i) :--: SCTimestamp
typeCheckCValue (Un.ValueBytes (Un.InternalByteString s)) CBytes =
  pure $ CvBytes s :--: SCBytes
typeCheckCValue Un.ValueTrue CBool = pure $ CvBool True :--: SCBool
typeCheckCValue Un.ValueFalse CBool = pure $ CvBool False :--: SCBool
typeCheckCValue _ _ = Nothing

typeCheckCValues
  :: forall t op . Typeable t
  => [Un.Value op]
  -> CT
  -> Either (Un.Value op, Text) [CValue t]
typeCheckCValues mvs t = traverse check mvs
  where
    check mv = do
      v :--: (_ :: Sing t') <-
        maybe (Left (mv, "failed to typecheck cval")) pure $ typeCheckCValue mv t
      Refl <- eqT' @t @t' `onLeft` (,) mv
      pure v

-- | Function @typeCheckValueImpl@ converts a single Michelson value
-- given in representation from @Michelson.Type@ module to representation
-- in strictly typed GADT.
--
-- As a third argument, @typeCheckValueImpl@ accepts expected type of value.
--
-- Type checking algorithm pattern-matches on parse value representation,
-- expected type @t@ and constructs @Val t@ value.
--
-- If there was no match on a given pair of value and expected type,
-- that is interpreted as input of wrong type and type check finishes with
-- error.
typeCheckValueImpl
  :: Show InstrExtT
  => TcInstrHandler
  -> Un.Value Un.Op
  -> T
  -> TypeCheckT SomeVal
typeCheckValueImpl _ mv t@(Tc ct) =
  maybe (throwError $ TCFailedOnValue mv t "")
        (\(v :--: cst) -> pure $ VC v :::: (STc cst, NStar))
        (typeCheckCValue mv ct)
typeCheckValueImpl _ (Un.ValueString (parsePublicKey -> Right s)) TKey =
  pure $ VKey s :::: (STKey, NStar)

typeCheckValueImpl _ (Un.ValueString (parseSignature -> Right s)) TSignature =
  pure $ VSignature s :::: (STSignature, NStar)

typeCheckValueImpl _ (Un.ValueString (parseAddress -> Right s)) (TContract pt) =
  withSomeSingT pt $ \p ->
    pure $ VContract s :::: (STContract p, NStar)
typeCheckValueImpl _ Un.ValueUnit TUnit = pure $ VUnit :::: (STUnit, NStar)
typeCheckValueImpl tcDo (Un.ValuePair ml mr) (TPair lt rt) = do
  l :::: (lst, ln) <- typeCheckValueImpl tcDo ml lt
  r :::: (rst, rn) <- typeCheckValueImpl tcDo mr rt
  let ns = mkNotes $ NTPair def def def ln rn
  pure $ VPair (l, r) :::: (STPair lst rst, ns)
typeCheckValueImpl tcDo (Un.ValueLeft ml) (TOr lt rt) = do
  l :::: (lst, ln) <- typeCheckValueImpl tcDo ml lt
  withSomeSingT rt $ \rst ->
    pure $ VOr (Left l) :::: ( STOr lst rst
                             , mkNotes $ NTOr def def def ln NStar )
typeCheckValueImpl tcDo (Un.ValueRight mr) (TOr lt rt) = do
  r :::: (rst, rn) <- typeCheckValueImpl tcDo mr rt
  withSomeSingT lt $ \lst ->
    pure $ VOr (Right r) :::: ( STOr lst rst
                              , mkNotes $ NTOr def def def NStar rn )
typeCheckValueImpl tcDo (Un.ValueSome mv) (TOption vt) = do
  v :::: (vst, vns) <- typeCheckValueImpl tcDo mv vt
  let ns = mkNotes $ NTOption def def vns
  pure $ VOption (Just v) :::: (STOption vst, ns)
typeCheckValueImpl _ Un.ValueNone (TOption vt) =
  withSomeSingT vt $ \vst ->
    pure $ VOption Nothing :::: (STOption vst, NStar)

typeCheckValueImpl tcDo (Un.ValueSeq mels) (TList vt) =
  withSomeSingT vt $ \vst -> do
    (els, ns) <- typeCheckValuesImpl tcDo mels vt
    pure $ VList els :::: (STList vst, mkNotes $ NTList def ns)

typeCheckValueImpl _ (Un.ValueSeq mels) (TSet vt) =
  withSomeSingCT vt $ \vst -> do
    els <- liftEither $ typeCheckCValues mels vt
            `onLeft` \(cv, err) -> TCFailedOnValue cv (Tc vt) $
                                      "wrong type of set element: " <> err
    pure $ VSet (S.fromList els) :::: (STSet vst, NStar)

typeCheckValueImpl tcDo (Un.ValueMap mels) (TMap kt vt) =
  withSomeSingT vt $ \vst ->
  withSomeSingCT kt $ \kst -> do
    ks <- liftEither $  typeCheckCValues (map (\(Un.Elt k _) -> k) mels) kt
            `onLeft` \(cv, err) -> TCFailedOnValue cv (Tc kt) $
                                      "wrong type of map key: " <> err
    (vals, vns) <- typeCheckValuesImpl tcDo (map (\(Un.Elt _ v) -> v) mels) vt
    let ns = mkNotes $ NTMap def def vns
    pure $ VMap (M.fromList $ zip ks vals) :::: (STMap kst vst, ns)

typeCheckValueImpl tcDo v@(Un.ValueLambda (fmap Un.unOp -> mp)) t@(TLambda mi mo) =
  withSomeSingT mi $ \(it :: Sing it) ->
  withSomeSingT mo $ \(ot :: Sing ot) ->
    typeCheckImpl tcDo mp (SomeHST $ (it, NStar, def) ::& SNil) >>= \case
      SiFail -> pure $ VLam FAILWITH :::: (STLambda it ot, NStar)
      lam ::: ((li :: HST li), (lo :: HST lo)) -> do
        Refl <- liftEither $ eqT' @li @'[ it ] `onLeft` unexpectedErr
        case (eqT' @'[ ot ] @lo, SomeHST lo, SomeHST li) of
          (Right Refl,
           SomeHST ((_, ons, _) ::& SNil :: HST lo'),
           SomeHST ((_, ins, _) ::& SNil :: HST li')) -> do
            Refl <- liftEither $ eqT' @lo @lo' `onLeft` unexpectedErr
            Refl <- liftEither $ eqT' @li @li' `onLeft` unexpectedErr
            let ns = mkNotes $ NTLambda def ins ons
            pure $ VLam lam :::: (STLambda it ot, ns)
          (Right _, _, _) ->
            throwError $ TCFailedOnValue v t
                    "wrong output type of lambda's value (wrong stack size)"
          (Left m, _, _) ->
            throwError $ TCFailedOnValue v t $
                    "wrong output type of lambda's value: " <> m
  where
    unexpectedErr m = TCFailedOnValue v t ("unexpected " <> m)

typeCheckValueImpl _ v t = throwError $ TCFailedOnValue v t ""

typeCheckValuesImpl
  :: forall t . (Typeable t, Show InstrExtT)
  => TcInstrHandler
  -> [Un.Value Un.Op]
  -> T
  -> TypeCheckT ([Value Instr t], Notes t)
typeCheckValuesImpl tcDo mvs t = foldM check ([], NStar) mvs
  where
    check (res, ns) mv = do
      v :::: ((_ :: Sing t'), vns) <- typeCheckValueImpl tcDo mv t
      Refl <- liftEither $ eqT' @t @t'
                `onLeft` (TCFailedOnValue mv t . ("wrong element type " <>))
      ns' <- liftEither $ converge ns vns `onLeft` TCFailedOnValue mv t
      pure (v : res, ns')
