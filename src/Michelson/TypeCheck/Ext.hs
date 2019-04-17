-- | Type-checking of Morley extension.
module Michelson.TypeCheck.Ext
  ( typeCheckExt
  ) where

import Control.Monad.Except (liftEither, throwError)
import Data.Map.Lazy (Map, insert, lookup)
import qualified Data.Map.Lazy as Map
import Data.Singletons (Sing)
import Data.Typeable ((:~:)(..))

import Michelson.TypeCheck.Error
import Michelson.TypeCheck.Helpers
import Michelson.TypeCheck.TypeCheck
import Michelson.TypeCheck.Types
import Michelson.Typed (converge, extractNotes, mkUType)
import qualified Michelson.Typed as T
import Michelson.Untyped (CT(..))
import qualified Michelson.Untyped as U
import Morley.Types
import Util.Peano (Sing(SS, SZ))

type TypeCheckListHandler inp =
     [U.ExpandedOp]
  -> HST inp
  -> TypeCheckT (SomeInstr inp)

typeCheckExt
  :: forall s.
     (Typeable s)
  => TypeCheckListHandler s
  -> ExpandedUExtInstr
  -> TcExtFrames
  -> HST s
  -> TypeCheckT (TcExtFrames, Maybe (ExtInstr s))
typeCheckExt typeCheckListH ext nfs hst =
  case ext of
    STACKTYPE s -> fitError $ (nfs, Nothing) <$ checkStackType noBoundVars s hst
    FN t sf     -> fitError $ (, Nothing) <$> checkFn t sf hst nfs
    FN_END      -> fitError $ (safeTail nfs, Nothing) <$ checkFnEnd hst nfs
    UPRINT pc   -> verifyPrint pc <&> \tpc -> (nfs, Just $ PRINT tpc)
    UTEST_ASSERT UTestAssert{..} -> do
      verifyPrint tassComment
      _ :/ si <- typeCheckListH tassInstrs hst
      case si of
        AnyOutInstr _ -> throwError $ TCExtError (SomeHST hst) $ TestAssertError
                         "TEST_ASSERT has to return Bool, but it always fails"
        instr ::: (((_ :: (Sing b, T.Notes b, VarAnn)) ::& (_ :: HST out1))) -> do
          Refl <- liftEither $
                    first (const $ TCExtError (SomeHST hst) $
                           TestAssertError "TEST_ASSERT has to return Bool, but returned something else") $
                      eqType @b @('T.Tc 'CBool)
          tcom <- verifyPrint tassComment
          pure (nfs, Just $ TEST_ASSERT $ TestAssert tassName tcom instr)
        _ -> throwError $ TCExtError (SomeHST hst) $ TestAssertError "TEST_ASSERT has to return Bool, but the stack is empty"
  where
    verifyPrint :: UPrintComment -> TypeCheckT (PrintComment s)
    verifyPrint (UPrintComment pc) = do
      let checkStRef (Left txt) = pure (Left txt)
          checkStRef (Right (UStackRef i)) = Right <$> do
            liftEither $ createStackRef i hst
      PrintComment <$> traverse checkStRef pc

    safeTail :: [a] -> [a]
    safeTail (_:as) = as
    safeTail [] = []

    fitError = liftEither . first (TCExtError (SomeHST hst))

-- | Check that a @StackTypePattern@ matches the type of the current stack
checkStackType :: Typeable xs => BoundVars -> StackTypePattern -> HST xs
               -> Either ExtError BoundVars
checkStackType (BoundVars vars boundStkRest) s it = go vars 0 s it
  where
    go :: Typeable xs => Map Var Type -> Int -> StackTypePattern -> HST xs
       -> Either ExtError BoundVars
    go m _ StkRest sr = case boundStkRest of
      Nothing -> pure $ BoundVars m (Just $ SomeHST sr)
      Just si@(SomeHST sr') ->
        bimap (StkRestMismatch s (SomeHST sr) si)
              (const $ BoundVars m (Just si))
              (eqHST sr sr')
    go m _ StkEmpty SNil = pure $ BoundVars m Nothing
    go _ _ StkEmpty _    = Left $ LengthMismatch s
    go _ _ _ SNil        = Left $ LengthMismatch s
    go m n (StkCons (TyCon t) ts) ((xt, xann, _) ::& xs) = do
      tann <- first (TypeMismatch s n . ExtractionTypeMismatch) (extractNotes t xt)
      void $ first (TypeMismatch s n . AnnError) (converge tann xann)
      go m (n + 1) ts xs
    go m n (StkCons (VarID v) ts) ((xt, xann, _) ::& xs) =
      case lookup v m of
        Nothing -> let t = mkUType xt xann in go (insert v t m) (n + 1) ts xs
        Just t -> do
          tann <- first (TyVarMismatch v t s n . ExtractionTypeMismatch) (extractNotes t xt)
          void $ first (TyVarMismatch v t s n . AnnError) (converge tann xann)
          go m (n + 1) ts xs

-- | Check that the optional "forall" variables are consistent if present
checkVars :: Text -> StackFn -> Either ExtError ()
checkVars t sf = case quantifiedVars sf of
  Just qs
    | varSet (inPattern sf) /= qs -> Left $ VarError t sf
  _ -> pure ()

-- | Checks the pattern in @FN@ and pushes a @ExtFrame@ onto the state
checkFn
  :: Typeable s
  => Text -> StackFn -> HST s -> TcExtFrames -> Either ExtError TcExtFrames
checkFn t sf it nfs = do
  checkVars t sf
  second (const $ (FN t sf, SomeHST it) : nfs) (checkStackType noBoundVars (inPattern sf) it)

-- |  Pops a @ExtFrame@ off the state and checks an @FN_END@ based on it
checkFnEnd :: Typeable s => HST s -> TcExtFrames -> Either ExtError BoundVars
checkFnEnd it' (nf@(nop, SomeHST it):_) = case nop of
  FN t sf -> do
    checkVars t sf
    m <- checkStackType noBoundVars (inPattern sf) it
    checkStackType m (outPattern sf) it'
  _ -> Left $ FnEndMismatch (Just nf)
checkFnEnd _ _ = Left $ FnEndMismatch Nothing

data BoundVars = BoundVars (Map Var Type) (Maybe SomeHST)

noBoundVars :: BoundVars
noBoundVars = BoundVars Map.empty Nothing

eqHST :: (Typeable as, Typeable bs) => HST as -> HST bs -> Either TCTypeError (as :~: bs)
eqHST (it :: HST xs) (it' :: HST ys) = do
  Refl <- (eqType @xs @ys)
  void $ convergeHST it it' `onLeft` AnnError
  return Refl

lengthHST :: HST xs -> Natural
lengthHST (_ ::& xs) = 1 + lengthHST xs
lengthHST SNil = 0

-- | Create stack reference accessing element with a given index.
--
-- Fails when index is too large for the given stack.
createStackRef :: Typeable s => Natural -> HST s -> Either TCError (StackRef s)
createStackRef idx hst =
  case doCreate (hst, idx) of
    Just sr -> Right sr
    Nothing -> Left $
      TCExtError (SomeHST hst) $
      InvalidStackReference (UStackRef idx) (StackSize $ lengthHST hst)
  where
    doCreate :: forall s. (HST s, Natural) -> Maybe (StackRef s)
    doCreate = \case
      (SNil, _) -> Nothing
      ((_ ::& _), 0) -> Just (StackRef SZ)
      ((_ ::& st), i) -> do
        StackRef ns <- doCreate (st, i - 1)
        return $ StackRef (SS ns)
