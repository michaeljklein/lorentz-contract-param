{-# OPTIONS_GHC -fno-warn-orphans #-}

module Michelson.Typed.Convert
  ( convertContract
  , instrToOp
  , instrToOps
  , valToValue
  , Conversible (..)
  , ConversibleExt
  ) where

import qualified Data.Map as Map
import Data.Singletons (SingI(sing))

import Michelson.Typed.CValue
import Michelson.Typed.Extract (toUType)
import Michelson.Typed.Instr as Instr
import Michelson.Typed.Scope
import Michelson.Typed.Sing (Sing(..), fromSingCT, fromSingT)
import Michelson.Typed.T (CT(..), T(..))
import Michelson.Typed.Value
import qualified Michelson.Untyped as Un
import Tezos.Address (formatAddress)
import Tezos.Core (unMutez)
import Tezos.Crypto (formatKeyHash, formatPublicKey, formatSignature)

class Conversible ext1 ext2 where
  convert :: ext1 -> ext2

type ConversibleExt = Conversible (ExtT Instr) (Un.ExtU Un.InstrAbstract Un.ExpandedOp)

convertContract
  :: forall param store . (SingI param, SingI store, ConversibleExt)
  => Contract param store -> Un.UntypedContract
convertContract contract =
  Un.Contract
    { para = toUType $ fromSingT (sing @param)
    , stor = toUType $ fromSingT (sing @store)
    , code = instrToOp contract
    }

-- | Convert a typed 'Val' to an untyped 'Value'.
--
-- For full isomorphism type of the given 'Val' should not contain
-- 'TOperation' - a compile error will be raised otherwise.
-- You can analyse its presence with 'checkOpPresence' function.
valToValue ::
     forall t . (ConversibleExt, SingI t, HasNoOp t)
  => Val Instr t
  -> Un.UntypedValue
valToValue val = case (val, sing @t) of
  (VC cVal, _) ->
    cValToValue cVal
  (VKey b, _) ->
    Un.ValueString $ formatPublicKey b
  (VUnit, _) ->
    Un.ValueUnit
  (VSignature b, _) ->
    Un.ValueString $ formatSignature b
  (VOption (Just x), STOption _) ->
    Un.ValueSome (valToValue x)
  (VOption Nothing, STOption _) ->
    Un.ValueNone
  (VList l, STList _) ->
    vList Un.ValueSeq $ map valToValue l
  (VSet s, _) ->
    vList Un.ValueSeq $ map cValToValue $ toList s
  (VContract b, _) ->
    Un.ValueString $ formatAddress b

  (VPair (l, r), STPair lt _) ->
    case checkOpPresence lt of
      OpAbsent -> Un.ValuePair (valToValue l) (valToValue r)

  (VOr (Left x), STOr lt _) ->
    case checkOpPresence lt of
      OpAbsent -> Un.ValueLeft (valToValue x)

  (VOr (Right x), STOr lt _) ->
    case checkOpPresence lt of
      OpAbsent -> Un.ValueRight (valToValue x)

  (VLam ops, _) ->
    vList Un.ValueLambda $ instrToOps ops

  (VMap m, STMap _ vt) ->
    case checkOpPresence vt of
      OpAbsent ->
        vList Un.ValueMap $ Map.toList m <&> \(k, v) ->
        Un.Elt (cValToValue k) (valToValue v)

  (VBigMap m, STBigMap _ vt) ->
    case checkOpPresence vt of
      OpAbsent ->
        vList Un.ValueMap $ Map.toList m <&> \(k, v) ->
        Un.Elt (cValToValue k) (valToValue v)
  where
    vList ctor = maybe Un.ValueNil ctor . nonEmpty

cValToValue :: CVal t -> Un.UntypedValue
cValToValue cVal = case cVal of
  CvInt i -> Un.ValueInt i
  CvNat i -> Un.ValueInt $ toInteger i
  CvString s -> Un.ValueString s
  CvBytes b -> Un.ValueBytes $ Un.InternalByteString b
  CvMutez m -> Un.ValueInt $ toInteger $ unMutez m
  CvBool True -> Un.ValueTrue
  CvBool False -> Un.ValueFalse
  CvKeyHash h -> Un.ValueString $ formatKeyHash h
  CvTimestamp t -> Un.ValueString $ show t
  CvAddress a -> Un.ValueString $ formatAddress a

instrToOp :: ConversibleExt => Instr inp out -> Un.ExpandedOp
instrToOp = Un.SeqEx . instrToOps

instrToOps :: ConversibleExt => Instr inp out -> [Un.ExpandedOp]
instrToOps instr = case instr of
  Nested sq -> one $ instrToOp sq
  i -> Un.PrimEx <$> handleInstr i
  where
    handleInstr :: Instr inp out -> [Un.ExpandedInstr]
    handleInstr (Seq i1 i2) = handleInstr i1 <> handleInstr i2
    handleInstr Nop = []
    handleInstr (Ext nop) = [Un.EXT $ convert nop]
    handleInstr (Nested _) = error "impossible"
    handleInstr DROP = [Un.DROP]
    handleInstr DUP = [Un.DUP Un.noAnn]
    handleInstr SWAP = [Un.SWAP]
    handleInstr i@(PUSH val) = handle i
      where
        handle :: Instr inp1 (t ': s) -> [Un.ExpandedInstr]
        handle (PUSH _ :: Instr inp1 (t ': s)) =
          let value = valToValue val
          in [Un.PUSH Un.noAnn (toUType $ fromSingT (sing @t)) value]
        handle _ = error "unexcepted call"
    handleInstr i@NONE = handle i
      where
        handle :: Instr inp1 ('TOption a ': inp1) -> [Un.ExpandedInstr]
        handle (NONE :: Instr inp1 ('TOption a ': inp1)) =
          [Un.NONE Un.noAnn Un.noAnn Un.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr SOME = [Un.SOME Un.noAnn Un.noAnn Un.noAnn]
    handleInstr UNIT = [Un.UNIT Un.noAnn Un.noAnn]
    handleInstr (IF_NONE i1 i2) = [Un.IF_NONE (instrToOp i1) (instrToOp i2)]
    handleInstr PAIR = [Un.PAIR Un.noAnn Un.noAnn Un.noAnn Un.noAnn]
    handleInstr CAR = [Un.CAR Un.noAnn Un.noAnn]
    handleInstr CDR = [Un.CDR Un.noAnn Un.noAnn]
    handleInstr i@LEFT = handle i
      where
        handle :: Instr (a ': s) ('TOr a b ': s) -> [Un.ExpandedInstr]
        handle (LEFT :: Instr (a ': s) ('TOr a b ': s)) =
          [Un.LEFT Un.noAnn Un.noAnn Un.noAnn Un.noAnn (toUType $ fromSingT (sing @b))]
        handle _ = error "unexcepted call"
    handleInstr i@(RIGHT) = handle i
      where
        handle :: Instr (b ': s) ('TOr a b ': s) -> [Un.ExpandedInstr]
        handle (RIGHT :: Instr (b ': s) ('TOr a b ': s)) =
          [Un.RIGHT Un.noAnn Un.noAnn Un.noAnn Un.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr (IF_LEFT i1 i2) = [Un.IF_LEFT (instrToOp i1) (instrToOp i2)]
    handleInstr i@NIL = handle i
      where
        handle :: Instr s ('TList p ': s) -> [Un.ExpandedInstr]
        handle (NIL :: Instr s ('TList p ': s)) =
          [Un.NIL Un.noAnn Un.noAnn (toUType $ fromSingT (sing @p))]
        handle _ = error "unexcepted call"
    handleInstr CONS = [Un.CONS Un.noAnn]
    handleInstr (IF_CONS i1 i2) = [Un.IF_CONS (instrToOp i1) (instrToOp i2)]
    handleInstr SIZE = [Un.SIZE Un.noAnn]
    handleInstr i@EMPTY_SET = handle i
      where
        handle :: Instr s ('TSet e ': s) -> [Un.ExpandedInstr]
        handle (EMPTY_SET :: Instr s ('TSet e ': s)) =
          [Un.EMPTY_SET Un.noAnn Un.noAnn (Un.Comparable (fromSingCT (sing @e)) Un.noAnn)]
        handle _ = error "unexcepted call"
    handleInstr i@EMPTY_MAP = handle i
      where
        handle :: Instr s ('TMap a b ': s) -> [Un.ExpandedInstr]
        handle (EMPTY_MAP :: Instr s ('TMap a b ': s)) =
          [Un.EMPTY_MAP Un.noAnn Un.noAnn (Un.Comparable (fromSingCT (sing @a)) Un.noAnn)
           (toUType $ fromSingT (sing @b))
          ]
        handle _ = error "unexcepted call"
    handleInstr (MAP op) = [Un.MAP Un.noAnn $ instrToOp op]
    handleInstr (ITER op) = [Un.ITER $ instrToOp op]
    handleInstr MEM = [Un.MEM Un.noAnn]
    handleInstr GET = [Un.GET Un.noAnn]
    handleInstr UPDATE = [Un.UPDATE]
    handleInstr (IF op1 op2) = [Un.IF (instrToOp op1) (instrToOp op2)]
    handleInstr (LOOP op) = [Un.LOOP (instrToOp op)]
    handleInstr (LOOP_LEFT op) = [Un.LOOP_LEFT (instrToOp op)]
    handleInstr i@(LAMBDA l) = handle i
      where
        handle :: Instr s ('TLambda i o ': s) -> [Un.ExpandedInstr]
        handle (LAMBDA _ :: Instr s ('TLambda i o ': s)) =
          [Un.LAMBDA Un.noAnn (toUType $ fromSingT (sing @i))
            (toUType $ fromSingT (sing @i)) (convertLambdaBody l)
          ]
        handle _ = error "unexcepted call"
        convertLambdaBody :: Val Instr ('TLambda i o) -> Un.ExpandedOp
        convertLambdaBody (VLam ops) = instrToOp ops
    handleInstr EXEC = [Un.EXEC Un.noAnn]
    handleInstr (DIP op) = [Un.DIP (instrToOp op)]
    handleInstr FAILWITH = [Un.FAILWITH]
    handleInstr i@(CAST) = handle i
      where
        handle :: Instr (a ': s) (a ': s) -> [Un.ExpandedInstr]
        handle (CAST :: Instr (a ': s) (a ': s)) =
          [Un.CAST Un.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr RENAME = [Un.RENAME Un.noAnn]
    handleInstr PACK = [Un.PACK Un.noAnn]
    handleInstr i@(UNPACK) = handle i
      where
        handle :: Instr ('Tc 'CBytes ': s) ('TOption a ': s) -> [Un.ExpandedInstr]
        handle (UNPACK :: Instr ('Tc 'CBytes ': s) ('TOption a ': s)) =
          [Un.UNPACK Un.noAnn (toUType $ fromSingT (sing @a))]
        handle _ = error "unexcepted call"
    handleInstr CONCAT = [Un.CONCAT Un.noAnn]
    handleInstr CONCAT' = [Un.CONCAT Un.noAnn]
    handleInstr SLICE = [Un.SLICE Un.noAnn]
    handleInstr ISNAT = [Un.ISNAT Un.noAnn]
    handleInstr ADD = [Un.ADD Un.noAnn]
    handleInstr SUB = [Un.SUB Un.noAnn]
    handleInstr MUL = [Un.MUL Un.noAnn]
    handleInstr EDIV = [Un.EDIV Un.noAnn]
    handleInstr ABS = [Un.ABS Un.noAnn]
    handleInstr NEG = [Un.NEG]
    handleInstr LSL = [Un.LSL Un.noAnn]
    handleInstr LSR = [Un.LSR Un.noAnn]
    handleInstr OR = [Un.OR Un.noAnn]
    handleInstr AND = [Un.AND Un.noAnn]
    handleInstr XOR = [Un.XOR Un.noAnn]
    handleInstr NOT = [Un.NOT Un.noAnn]
    handleInstr COMPARE = [Un.COMPARE Un.noAnn]
    handleInstr Instr.EQ = [Un.EQ Un.noAnn]
    handleInstr NEQ = [Un.NEQ Un.noAnn]
    handleInstr Instr.LT = [Un.LT Un.noAnn]
    handleInstr Instr.GT = [Un.GT Un.noAnn]
    handleInstr LE = [Un.LE Un.noAnn]
    handleInstr GE = [Un.GE Un.noAnn]
    handleInstr INT = [Un.INT Un.noAnn]
    handleInstr SELF = [Un.SELF Un.noAnn]
    handleInstr i@CONTRACT = handle i
      where
        handle :: Instr ('Tc 'CAddress ': s) ('TOption ('TContract p) ': s)
               -> [Un.ExpandedInstr]
        handle (CONTRACT :: Instr ('Tc 'CAddress ': s) ('TOption ('TContract p) ': s)) =
          [Un.CONTRACT Un.noAnn (toUType $ fromSingT (sing @p))]
        handle _ = error "unexcepted call"
    handleInstr TRANSFER_TOKENS = [Un.TRANSFER_TOKENS Un.noAnn]
    handleInstr SET_DELEGATE = [Un.SET_DELEGATE Un.noAnn]
    handleInstr CREATE_ACCOUNT = [Un.CREATE_ACCOUNT Un.noAnn Un.noAnn]
    handleInstr CREATE_CONTRACT = [Un.CREATE_CONTRACT Un.noAnn Un.noAnn]
    handleInstr i@(CREATE_CONTRACT2 _) = handle i
      where
        handle :: Instr ('Tc 'CKeyHash ': 'TOption ('Tc 'CKeyHash)
                    ': 'Tc 'CBool ': 'Tc 'CBool ': 'Tc 'CMutez ': g ': s)
                   ('TOperation ': 'Tc 'CAddress ': s) -> [Un.ExpandedInstr]
        handle (CREATE_CONTRACT2 ops :: Instr ('Tc 'CKeyHash
                    ': 'TOption ('Tc 'CKeyHash)
                    ': 'Tc 'CBool ': 'Tc 'CBool ': 'Tc 'CMutez ': g ': s)
                   ('TOperation ': 'Tc 'CAddress ': s)) =
          case ops of
            (code :: Instr '[ 'TPair p g ] '[ 'TPair ('TList 'TOperation) g ]) ->
              let contract = Un.Contract (toUType $ fromSingT (sing @p))
                    (toUType $ fromSingT (sing @g)) (instrToOp code) in
              [Un.CREATE_CONTRACT2 Un.noAnn Un.noAnn contract]
        handle _ = error "unexcepted call"
    handleInstr IMPLICIT_ACCOUNT = [Un.IMPLICIT_ACCOUNT Un.noAnn]
    handleInstr NOW = [Un.NOW Un.noAnn]
    handleInstr AMOUNT = [Un.AMOUNT Un.noAnn]
    handleInstr BALANCE = [Un.BALANCE Un.noAnn]
    handleInstr CHECK_SIGNATURE = [Un.CHECK_SIGNATURE Un.noAnn]
    handleInstr SHA256 = [Un.SHA256 Un.noAnn]
    handleInstr SHA512 = [Un.SHA512 Un.noAnn]
    handleInstr BLAKE2B = [Un.BLAKE2B Un.noAnn]
    handleInstr HASH_KEY = [Un.HASH_KEY Un.noAnn]
    handleInstr STEPS_TO_QUOTA = [Un.STEPS_TO_QUOTA Un.noAnn]
    handleInstr SOURCE = [Un.SOURCE Un.noAnn]
    handleInstr SENDER = [Un.SENDER Un.noAnn]
    handleInstr ADDRESS = [Un.ADDRESS Un.noAnn]

-- It's an orphan instance, but it's better than checking all cases manually.
-- We can also move this convertion to the place where `Instr` is defined,
-- but then there will be a very large module (as we'll have to move a lot of
-- stuff as well).
instance (ConversibleExt, Eq Un.ExpandedInstrExtU) => Eq (Instr inp out) where
  i1 == i2 = instrToOps i1 == instrToOps i2
