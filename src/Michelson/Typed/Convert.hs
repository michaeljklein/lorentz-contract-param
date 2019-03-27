{-# OPTIONS_GHC -fno-warn-orphans #-}

module Michelson.Typed.Convert
  ( convertContract
  , instrToOps
  , unsafeValToValue
  , valToOpOrValue
  , Conversible (..)
  , ConversibleExt
  ) where

import qualified Data.Map as Map
import Data.Singletons (SingI(sing))

import Michelson.Typed.CValue
import Michelson.Typed.Extract (toUType)
import Michelson.Typed.Instr as Instr
import Michelson.Typed.Sing (fromSingCT, fromSingT)
import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.Value

import qualified Michelson.Untyped as U
import Tezos.Address (formatAddress)
import Tezos.Core (unMutez)
import Tezos.Crypto (formatKeyHash, formatPublicKey, formatSignature)

class Conversible ext1 ext2 where
  convert :: ext1 -> ext2

type ConversibleExt = Conversible (ExtT Instr) (U.ExtU U.InstrAbstract U.Op)

convertContract
  :: forall param store . (SingI param, SingI store, ConversibleExt)
  => Contract param store -> U.Contract
convertContract contract =
  U.Contract
    { para = toUType $ fromSingT (sing @param)
    , stor = toUType $ fromSingT (sing @store)
    , code = instrToOps contract
    }

-- | Function @unsafeValToValue@ converts typed @Val@ to untyped @Value@
-- from @Michelson.Untyped.Value@ module
--
-- VOp cannot be represented in @Value@ from untyped types, so calling this function
-- on it will cause an error
unsafeValToValue :: (ConversibleExt, HasCallStack) => Value' Instr t -> U.Value
unsafeValToValue = fromMaybe (error err) . valToOpOrValue
  where
    err =
      "unexpected unsafeValToValue call trying to convert VOp to untyped Value"

-- | Convert a typed 'Val' to an untyped 'Value', or fail if it contains operations
-- which are unrepresentable there.
valToOpOrValue :: forall t . ConversibleExt => Value' Instr t -> Maybe U.Value
valToOpOrValue = \case
  VC cVal -> Just $ cValToValue cVal
  VKey b -> Just $ U.ValueString $ formatPublicKey b
  VUnit -> Just $ U.ValueUnit
  VSignature b -> Just $ U.ValueString $ formatSignature b
  VOption (Just x) -> U.ValueSome <$> valToOpOrValue x
  VOption Nothing -> Just $ U.ValueNone
  VList l -> U.ValueSeq <$> mapM valToOpOrValue l
  VSet s -> Just $ U.ValueSeq $ map cValToValue $ toList s
  VOp _op -> Nothing
  VContract b -> Just $ U.ValueString $ formatAddress b
  VPair (l, r) -> U.ValuePair <$> valToOpOrValue l <*> valToOpOrValue r
  VOr (Left x) -> U.ValueLeft <$> valToOpOrValue x
  VOr (Right x) -> U.ValueRight <$> valToOpOrValue x
  VLam ops -> Just $ U.ValueLambda $ instrToOps ops
  VMap m ->
    fmap U.ValueMap . forM (Map.toList m) $ \(k, v) ->
      U.Elt (cValToValue k) <$> valToOpOrValue v
  VBigMap m ->
    fmap U.ValueMap . forM (Map.toList m) $ \(k, v) ->
      U.Elt (cValToValue k) <$> valToOpOrValue v

cValToValue :: CValue t -> U.Value
cValToValue cVal = case cVal of
  CvInt i -> U.ValueInt i
  CvNat i -> U.ValueInt $ toInteger i
  CvString s -> U.ValueString s
  CvBytes b -> U.ValueBytes $ U.InternalByteString b
  CvMutez m -> U.ValueInt $ toInteger $ unMutez m
  CvBool True -> U.ValueTrue
  CvBool False -> U.ValueFalse
  CvKeyHash h -> U.ValueString $ formatKeyHash h
  CvTimestamp t -> U.ValueString $ show t
  CvAddress a -> U.ValueString $ formatAddress a

instrToOps :: ConversibleExt => Instr inp out -> [U.Op]
instrToOps instr = U.Op <$> handleInstr instr
  where
    handleInstr :: Instr inp out -> [U.Instr]
    handleInstr (Seq i1 i2) = handleInstr i1 <> handleInstr i2
    handleInstr Nop = []
    handleInstr (Ext nop) = [U.EXT $ convert nop]
    handleInstr DROP = [U.DROP]
    handleInstr DUP = [U.DUP U.noAnn]
    handleInstr SWAP = [U.SWAP]
    handleInstr i@(PUSH val) = handle i
      where
        handle :: Instr inp1 (t ': s) -> [U.Instr]
        handle (PUSH _ :: Instr inp1 (t ': s)) =
          let value = unsafeValToValue val
              --- ^ safe because PUSH cannot have operation as argument
          in [U.PUSH U.noAnn (toUType $ fromSingT (sing @t)) value]
        handle _ = error "unexcepted call"
    handleInstr i@NONE = handle i
      where
        handle :: Instr inp1 ('TOption a ': inp1) -> [U.Instr]
        handle (NONE :: Instr inp1 ('TOption a ': inp1)) =
          [U.NONE U.noAnn U.noAnn U.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr SOME = [U.SOME U.noAnn U.noAnn U.noAnn]
    handleInstr UNIT = [U.UNIT U.noAnn U.noAnn]
    handleInstr (IF_NONE i1 i2) = [U.IF_NONE (instrToOps i1) (instrToOps i2)]
    handleInstr PAIR = [U.PAIR U.noAnn U.noAnn U.noAnn U.noAnn]
    handleInstr CAR = [U.CAR U.noAnn U.noAnn]
    handleInstr CDR = [U.CDR U.noAnn U.noAnn]
    handleInstr i@LEFT = handle i
      where
        handle :: Instr (a ': s) ('TOr a b ': s) -> [U.Instr]
        handle (LEFT :: Instr (a ': s) ('TOr a b ': s)) =
          [U.LEFT U.noAnn U.noAnn U.noAnn U.noAnn (toUType $ fromSingT (sing @b))]
        handle _ = error "unexcepted call"
    handleInstr i@(RIGHT) = handle i
      where
        handle :: Instr (b ': s) ('TOr a b ': s) -> [U.Instr]
        handle (RIGHT :: Instr (b ': s) ('TOr a b ': s)) =
          [U.RIGHT U.noAnn U.noAnn U.noAnn U.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr (IF_LEFT i1 i2) = [U.IF_LEFT (instrToOps i1) (instrToOps i2)]
    handleInstr (IF_RIGHT i1 i2) = [U.IF_RIGHT (instrToOps i1) (instrToOps i2)]
    handleInstr i@(NIL) = handle i
      where
        handle :: Instr s ('TList p ': s) -> [U.Instr]
        handle (NIL :: Instr s ('TList p ': s)) =
          [U.NIL U.noAnn U.noAnn (toUType $ fromSingT (sing @p))]
        handle _ = error "unexcepted call"
    handleInstr CONS = [U.CONS U.noAnn]
    handleInstr (IF_CONS i1 i2) = [U.IF_CONS (instrToOps i1) (instrToOps i2)]
    handleInstr SIZE = [U.SIZE U.noAnn]
    handleInstr i@EMPTY_SET = handle i
      where
        handle :: Instr s ('TSet e ': s) -> [U.Instr]
        handle (EMPTY_SET :: Instr s ('TSet e ': s)) =
          [U.EMPTY_SET U.noAnn U.noAnn (U.Comparable (fromSingCT (sing @e)) U.noAnn)]
        handle _ = error "unexcepted call"
    handleInstr i@EMPTY_MAP = handle i
      where
        handle :: Instr s ('TMap a b ': s) -> [U.Instr]
        handle (EMPTY_MAP :: Instr s ('TMap a b ': s)) =
          [U.EMPTY_MAP U.noAnn U.noAnn (U.Comparable (fromSingCT (sing @a)) U.noAnn)
           (toUType $ fromSingT (sing @b))
          ]
        handle _ = error "unexcepted call"
    handleInstr (MAP op) = [U.MAP U.noAnn $ instrToOps op]
    handleInstr (ITER op) = [U.ITER $ instrToOps op]
    handleInstr MEM = [U.MEM U.noAnn]
    handleInstr GET = [U.GET U.noAnn]
    handleInstr UPDATE = [U.UPDATE]
    handleInstr (IF op1 op2) = [U.IF (instrToOps op1) (instrToOps op2)]
    handleInstr (LOOP op) = [U.LOOP (instrToOps op)]
    handleInstr (LOOP_LEFT op) = [U.LOOP_LEFT (instrToOps op)]
    handleInstr i@(LAMBDA l) = handle i
      where
        handle :: Instr s ('TLambda i o ': s) -> [U.Instr]
        handle (LAMBDA _ :: Instr s ('TLambda i o ': s)) =
          [U.LAMBDA U.noAnn (toUType $ fromSingT (sing @i))
            (toUType $ fromSingT (sing @i)) (convertLambdaBody l)
          ]
        handle _ = error "unexcepted call"
        convertLambdaBody :: Value' Instr ('TLambda i o) -> [U.Op]
        convertLambdaBody (VLam ops) = instrToOps ops
    handleInstr EXEC = [U.EXEC U.noAnn]
    handleInstr (DIP op) = [U.DIP (instrToOps op)]
    handleInstr FAILWITH = [U.FAILWITH]
    handleInstr i@(CAST) = handle i
      where
        handle :: Instr (a ': s) (a ': s) -> [U.Instr]
        handle (CAST :: Instr (a ': s) (a ': s)) =
          [U.CAST U.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr RENAME = [U.RENAME U.noAnn]
    handleInstr PACK = [U.PACK U.noAnn]
    handleInstr i@(UNPACK) = handle i
      where
        handle :: Instr ('Tc 'CBytes ': s) ('TOption a ': s) -> [U.Instr]
        handle (UNPACK :: Instr ('Tc 'CBytes ': s) ('TOption a ': s)) =
          [U.UNPACK U.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr CONCAT = [U.CONCAT U.noAnn]
    handleInstr CONCAT' = [U.CONCAT U.noAnn]
    handleInstr SLICE = [U.SLICE U.noAnn]
    handleInstr ISNAT = [U.ISNAT U.noAnn]
    handleInstr ADD = [U.ADD U.noAnn]
    handleInstr SUB = [U.SUB U.noAnn]
    handleInstr MUL = [U.MUL U.noAnn]
    handleInstr EDIV = [U.EDIV U.noAnn]
    handleInstr ABS = [U.ABS U.noAnn]
    handleInstr NEG = [U.NEG]
    handleInstr LSL = [U.LSL U.noAnn]
    handleInstr LSR = [U.LSR U.noAnn]
    handleInstr OR = [U.OR U.noAnn]
    handleInstr AND = [U.AND U.noAnn]
    handleInstr XOR = [U.XOR U.noAnn]
    handleInstr NOT = [U.NOT U.noAnn]
    handleInstr COMPARE = [U.COMPARE U.noAnn]
    handleInstr Instr.EQ = [U.EQ U.noAnn]
    handleInstr NEQ = [U.NEQ U.noAnn]
    handleInstr Instr.LT = [U.LT U.noAnn]
    handleInstr Instr.GT = [U.GT U.noAnn]
    handleInstr LE = [U.LE U.noAnn]
    handleInstr GE = [U.GE U.noAnn]
    handleInstr INT = [U.INT U.noAnn]
    handleInstr SELF = [U.SELF U.noAnn]
    handleInstr i@CONTRACT = handle i
      where
        handle :: Instr ('Tc 'CAddress ': s) ('TOption ('TContract p) ': s)
               -> [U.Instr]
        handle (CONTRACT :: Instr ('Tc 'CAddress ': s) ('TOption ('TContract p) ': s)) =
          [U.CONTRACT U.noAnn (toUType $ fromSingT (sing @p))]
        handle _ = error "unexcepted call"
    handleInstr TRANSFER_TOKENS = [U.TRANSFER_TOKENS U.noAnn]
    handleInstr SET_DELEGATE = [U.SET_DELEGATE U.noAnn]
    handleInstr CREATE_ACCOUNT = [U.CREATE_ACCOUNT U.noAnn U.noAnn]
    handleInstr CREATE_CONTRACT = [U.CREATE_CONTRACT U.noAnn U.noAnn]
    handleInstr i@(CREATE_CONTRACT2 _) = handle i
      where
        handle :: Instr ('Tc 'CKeyHash ': 'TOption ('Tc 'CKeyHash)
                    ': 'Tc 'CBool ': 'Tc 'CBool ': 'Tc 'CMutez ': g ': s)
                   ('TOperation ': 'Tc 'CAddress ': s) -> [U.Instr]
        handle (CREATE_CONTRACT2 ops :: Instr ('Tc 'CKeyHash
                    ': 'TOption ('Tc 'CKeyHash)
                    ': 'Tc 'CBool ': 'Tc 'CBool ': 'Tc 'CMutez ': g ': s)
                   ('TOperation ': 'Tc 'CAddress ': s)) =
          case ops of
            (code :: Instr '[ 'TPair p g ] '[ 'TPair ('TList 'TOperation) g ]) ->
              let contract = U.Contract (toUType $ fromSingT (sing @p))
                    (toUType $ fromSingT (sing @g)) (instrToOps code) in
              [U.CREATE_CONTRACT2 U.noAnn U.noAnn contract]
        handle _ = error "unexcepted call"
    handleInstr IMPLICIT_ACCOUNT = [U.IMPLICIT_ACCOUNT U.noAnn]
    handleInstr NOW = [U.NOW U.noAnn]
    handleInstr AMOUNT = [U.AMOUNT U.noAnn]
    handleInstr BALANCE = [U.BALANCE U.noAnn]
    handleInstr CHECK_SIGNATURE = [U.CHECK_SIGNATURE U.noAnn]
    handleInstr SHA256 = [U.SHA256 U.noAnn]
    handleInstr SHA512 = [U.SHA512 U.noAnn]
    handleInstr BLAKE2B = [U.BLAKE2B U.noAnn]
    handleInstr HASH_KEY = [U.HASH_KEY U.noAnn]
    handleInstr STEPS_TO_QUOTA = [U.STEPS_TO_QUOTA U.noAnn]
    handleInstr SOURCE = [U.SOURCE U.noAnn]
    handleInstr SENDER = [U.SENDER U.noAnn]
    handleInstr ADDRESS = [U.ADDRESS U.noAnn]

-- It's an orphan instance, but it's better than checking all cases manually.
-- We can also move this convertion to the place where `Instr` is defined,
-- but then there will be a very large module (as we'll have to move a lot of
-- stuff as well).
instance (ConversibleExt, Eq U.InstrExtU) => Eq (Instr inp out) where
  i1 == i2 = instrToOps i1 == instrToOps i2
