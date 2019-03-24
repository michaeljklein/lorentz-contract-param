{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Michelson.Typed.Convert
  ( convertContract
  , instrToOps
  , untypeValue
  , unsafeUntypeValue
  , Convertible (..)
  , ConvertibleExt
  ) where

import qualified Data.Map as Map
import Data.Singletons (SingI(sing))

import Michelson.Typed.CValue
import Michelson.Typed.Extract (toUType)
import Michelson.Typed.Instr as Instr
import Michelson.Typed.Instr (type (&), (:+>)(..))
import Michelson.Typed.Sing (fromSingCT, fromSingT)
import Michelson.Typed.T
  (TAddress, TBytes, TContract, TLambda, TList, TMap, TOperation, TOption, TOr, TPair, TSet)
import Michelson.Typed.Value
import qualified Michelson.Untyped as Un
import Tezos.Address (formatAddress)
import Tezos.Core (unMutez)
import Tezos.Crypto (formatKeyHash, formatPublicKey, formatSignature)

class Convertible ext1 ext2 where
  convert :: ext1 -> ext2

type ConvertibleExt = Convertible (ExtT Instr) (Un.ExtU Un.InstrAbstract Un.Op)

convertContract
  :: forall param store . (SingI param, SingI store, ConvertibleExt)
  => Contract param store -> Un.Contract Un.Op
convertContract contract =
  Un.Contract
    { para = toUType $ fromSingT (sing @param)
    , stor = toUType $ fromSingT (sing @store)
    , code = instrToOps contract
    }

-- | Function @unsafeValToValue@ converts typed @Val@ to untyped @Value@
-- from @Michelson.Untyped.Value@ module
--
-- VOp cannot be represented in @Value@ from untyped types, so calling this function
-- on it will cause an error
unsafeUntypeValue :: (ConvertibleExt, HasCallStack) => Value Instr t -> Un.Value Un.Op
unsafeUntypeValue = fromMaybe (error err) . untypeValue
  where
    err =
      "unexpected unsafeValToValue call trying to convert VOp to untyped Value"

-- | Convert a typed 'Val' to an untyped 'Value', or fail if it contains operations
-- which are unrepresentable there.
untypeValue ::
     forall t . ConvertibleExt
  => Value Instr t
  -> Maybe (Un.Value Un.Op)
untypeValue = \case
  VC cVal -> Just $ untypeCValue cVal
  VKey b -> Just $ Un.ValueString $ formatPublicKey b
  VUnit -> Just $ Un.ValueUnit
  VSignature b -> Just $ Un.ValueString $ formatSignature b
  VOption (Just x) -> Un.ValueSome <$> untypeValue x
  VOption Nothing -> Just $ Un.ValueNone
  VList l -> Un.ValueSeq <$> mapM untypeValue l
  VSet s -> Just $ Un.ValueSeq $ map untypeCValue $ toList s
  VOp _op -> Nothing
  VContract b -> Just $ Un.ValueString $ formatAddress b
  VPair (l, r) -> Un.ValuePair <$> untypeValue l <*> untypeValue r
  VOr (Left x) -> Un.ValueLeft <$> untypeValue x
  VOr (Right x) -> Un.ValueRight <$> untypeValue x
  VLam ops -> Just $ Un.ValueLambda $ instrToOps ops
  VMap m ->
    fmap Un.ValueMap . forM (Map.toList m) $ \(k, v) ->
      Un.Elt (untypeCValue k) <$> untypeValue v
  VBigMap m ->
    fmap Un.ValueMap . forM (Map.toList m) $ \(k, v) ->
      Un.Elt (untypeCValue k) <$> untypeValue v

untypeCValue :: CValue t -> Un.Value Un.Op
untypeCValue = \case
  CvInt i       -> Un.ValueInt i
  CvNat i       -> Un.ValueInt $ toInteger i
  CvString s    -> Un.ValueString s
  CvBytes b     -> Un.ValueBytes $ Un.InternalByteString b
  CvMutez m     -> Un.ValueInt $ toInteger $ unMutez m
  CvBool True   -> Un.ValueTrue
  CvBool False  -> Un.ValueFalse
  CvKeyHash h   -> Un.ValueString $ formatKeyHash h
  CvTimestamp t -> Un.ValueString $ show t
  CvAddress a   -> Un.ValueString $ formatAddress a

instrToOps :: ConvertibleExt => Instr inp out -> [Un.Op]
instrToOps = (fmap Un.Op) . untypeInstr

untypeInstr :: ConvertibleExt => Instr inp out -> [Un.Instr]
untypeInstr i = case i of
  Seq a b          -> untypeInstr a <> untypeInstr b
  Nop              -> []
  Ext nop          -> [Un.EXT $ convert nop]
  DROP             -> [Un.DROP]
  DUP              -> [Un.DUP noN]
  SWAP             -> [Un.SWAP]
  PUSH val         -> handlePush val
  NONE             -> handleNone i
  SOME             -> [Un.SOME noN noN noN]
  UNIT             -> [Un.UNIT noN noN]
  IF_NONE a b      -> [Un.IF_NONE (instrToOps a) (instrToOps b)]
  PAIR             -> [Un.PAIR noN noN noN noN]
  CAR              -> [Un.CAR noN noN]
  CDR              -> [Un.CDR noN noN]
  LEFT             -> handleLeft i
  RIGHT            -> handleRight i
  IF_LEFT a b      -> [Un.IF_LEFT (instrToOps a) (instrToOps b)]
  IF_RIGHT a b     -> [Un.IF_RIGHT (instrToOps a) (instrToOps b)]
  NIL              -> handleNil i
  CONS             -> [Un.CONS noN]
  IF_CONS a b      -> [Un.IF_CONS (instrToOps a) (instrToOps b)]
  SIZE             -> [Un.SIZE noN]
  EMPTY_SET        -> handleEmptySet i
  EMPTY_MAP        -> handleEmptyMap i
  MAP op           -> [Un.MAP noN $ instrToOps op]
  ITER op          -> [Un.ITER $ instrToOps op]
  MEM              -> [Un.MEM noN]
  GET              -> [Un.GET noN]
  UPDATE           -> [Un.UPDATE]
  IF a b           -> [Un.IF (instrToOps a) (instrToOps b)]
  LOOP op          -> [Un.LOOP (instrToOps op)]
  LOOP_LEFT op     -> [Un.LOOP_LEFT (instrToOps op)]
  LAMBDA l         -> handleLambda l
  EXEC             -> [Un.EXEC noN]
  DIP op           -> [Un.DIP (instrToOps op)]
  FAILWITH         -> [Un.FAILWITH]
  CAST             -> handleCast i
  RENAME           -> [Un.RENAME noN]
  PACK             -> [Un.PACK noN]
  UNPACK           -> handleUnpack i
  CONCAT           -> [Un.CONCAT noN]
  CONCAT'          -> [Un.CONCAT noN]
  SLICE            -> [Un.SLICE noN]
  ISNAT            -> [Un.ISNAT noN]
  ADD              -> [Un.ADD noN]
  SUB              -> [Un.SUB noN]
  MUL              -> [Un.MUL noN]
  EDIV             -> [Un.EDIV noN]
  ABS              -> [Un.ABS noN]
  NEG              -> [Un.NEG]
  LSL              -> [Un.LSL noN]
  LSR              -> [Un.LSR noN]
  OR               -> [Un.OR noN]
  AND              -> [Un.AND noN]
  XOR              -> [Un.XOR noN]
  NOT              -> [Un.NOT noN]
  COMPARE          -> [Un.COMPARE noN]
  Instr.EQ         -> [Un.EQ noN]
  NEQ              -> [Un.NEQ noN]
  Instr.LT         -> [Un.LT noN]
  Instr.GT         -> [Un.GT noN]
  LE               -> [Un.LE noN]
  GE               -> [Un.GE noN]
  INT              -> [Un.INT noN]
  SELF             -> [Un.SELF noN]
  CONTRACT         -> handleContract i
  TRANSFER_TOKENS  -> [Un.TRANSFER_TOKENS noN]
  SET_DELEGATE     -> [Un.SET_DELEGATE noN]
  CREATE_ACCOUNT   -> [Un.CREATE_ACCOUNT noN noN]
  CREATE_CONTRACT  -> [Un.CREATE_CONTRACT noN noN]
  CREATE_CONTRACT2 ops -> handleContractOps ops
  IMPLICIT_ACCOUNT -> [Un.IMPLICIT_ACCOUNT noN]
  NOW              -> [Un.NOW noN]
  AMOUNT           -> [Un.AMOUNT noN]
  BALANCE          -> [Un.BALANCE noN]
  CHECK_SIGNATURE  -> [Un.CHECK_SIGNATURE noN]
  SHA256           -> [Un.SHA256 noN]
  SHA512           -> [Un.SHA512 noN]
  BLAKE2B          -> [Un.BLAKE2B noN]
  HASH_KEY         -> [Un.HASH_KEY noN]
  STEPS_TO_QUOTA   -> [Un.STEPS_TO_QUOTA noN]
  SOURCE           -> [Un.SOURCE noN]
  SENDER           -> [Un.SENDER noN]
  ADDRESS          -> [Un.ADDRESS noN]
  where
    noN = Un.noAnn
    handlePush (val :: Value (:+>) t) =
       [Un.PUSH noN (toUType $ fromSingT (sing @t)) (unsafeUntypeValue val)]
           --- ^ safe because PUSH cannot have operation as argument
    handleNone (NONE :: s :+> TOption a & s) =
      [Un.NONE noN noN noN (toUType $ fromSingT (sing @a))]
    handleLeft (LEFT :: a & s :+> TOr a b & s) =
      [Un.LEFT noN noN noN noN (toUType $ fromSingT (sing @b))]
    handleRight (RIGHT :: b & s :+> TOr a b & s) =
      [Un.RIGHT noN noN noN noN (toUType $ fromSingT (sing @a))]
    handleNil (NIL :: s :+> TList p & s) =
      [Un.NIL noN noN (toUType $ fromSingT (sing @p))]
    handleEmptySet (EMPTY_SET :: s :+> TSet e & s) =
      [ Un.EMPTY_SET noN noN (Un.Comparable (fromSingCT (sing @e)) noN)]
    handleEmptyMap (EMPTY_MAP :: s :+> TMap a b & s) =
      [ Un.EMPTY_MAP noN noN (Un.Comparable (fromSingCT (sing @a)) noN)
        (toUType $ fromSingT (sing @b))
      ]
    handleLambda (l :: Value (:+>) (TLambda i o)) =
      [ Un.LAMBDA noN (toUType $ fromSingT (sing @i))
        (toUType $ fromSingT (sing @i)) (convertLambdaBody l)
      ]
    convertLambdaBody :: Value (:+>) (TLambda i o) -> [Un.Op]
    convertLambdaBody (VLam ops) = instrToOps ops
    handleCast (CAST :: (a & s) :+> (a & s)) =
      [Un.CAST noN (toUType $ fromSingT (sing @a))]
    handleUnpack (UNPACK :: TBytes & s :+> TOption a & s) =
      [Un.UNPACK noN (toUType $ fromSingT (sing @a))]
    handleContract
      (CONTRACT :: TAddress & s :+> TOption (TContract p) & s) =
        [Un.CONTRACT noN (toUType $ fromSingT (sing @p))]
    handleContractOps
      (ops :: '[ TPair p g ] :+> '[ TPair (TList TOperation) g ]) =
        [ Un.CREATE_CONTRACT2 noN noN $
          Un.Contract
            (toUType $ fromSingT (sing @p))
            (toUType $ fromSingT (sing @g)) (instrToOps ops)
        ]
