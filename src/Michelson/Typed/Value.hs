-- | Module, containing data types for Michelson value.

module Michelson.Typed.Value
  ( Value (..)
  , ContractInp
  , ContractOut
  , CreateAccount (..)
  , CreateContract (..)
  , CValue (..)
  , Operation (..)
  , SetDelegate (..)
  , TransferTokens (..)
  , Val
  , toVal
  , fromVal
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Singletons (SingI)

import Michelson.Typed.CValue (CVal, CValue(..), fromCVal, toCVal)
import Michelson.Typed.T
  (TBigMap, TContract, TKey, TLambda, TList, TMap, TOperation, TOption, TOr, TPair, TSet,
  TSignature, TUnit, Tc, ToT)
import Tezos.Address (Address)
import Tezos.Core (Mutez, Timestamp)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

-- | Data type, representing operation, list of which is returned
-- by Michelson contract (according to calling convention).
--
-- These operations are to be further executed against system state
-- after the contract execution.
data Operation instr where
  OpTransferTokens :: TransferTokens instr p -> Operation instr
  OpSetDelegate :: SetDelegate -> Operation instr
  OpCreateAccount :: CreateAccount -> Operation instr
  OpCreateContract
    :: (Show (instr (ContractInp cp st) (ContractOut st)), SingI cp, SingI st)
    => CreateContract instr t cp st
    -> Operation instr

deriving instance Show (Operation instr)

data TransferTokens instr p = TransferTokens
  { ttContractParameter :: !(Value instr p)
  , ttAmount :: !Mutez
  , ttContract :: !(Value instr (TContract p))
  } deriving (Show)

data SetDelegate = SetDelegate
  { sdMbKeyHash :: !(Maybe KeyHash)
  } deriving (Show)

data CreateAccount = CreateAccount
  { caManager :: !KeyHash
  , caDelegate :: !(Maybe KeyHash)
  , caSpendable :: !Bool
  , caBalance :: !Mutez
  } deriving (Show)

data CreateContract instr t cp st
  = Show (instr (ContractInp cp st) (ContractOut st))
  => CreateContract
  { ccManager :: !KeyHash
  , ccDelegate :: !(Maybe KeyHash)
  , ccSpendable :: !Bool
  , ccDelegatable :: !Bool
  , ccBalance :: !Mutez
  , ccStorageVal :: !(Value instr t)
  , ccContractCode :: !(instr (ContractInp cp st) (ContractOut st))
  }

deriving instance Show (CreateContract instr t cp st)

type ContractInp param st = '[ TPair param st ]
type ContractOut st = '[ TPair (TList TOperation) st ]

-- | Representation of Michelson value.
--
-- Type parameter @ins@ stands for Michelson instruction
-- type, i.e. data type to represent an instruction of language.
data Value ins t where
  VC         :: CValue t -> Value ins (Tc t)
  VKey       :: PublicKey -> Value ins TKey
  VUnit      :: Value ins TUnit
  VSignature :: Signature -> Value ins TSignature
  VOption    :: Maybe (Value ins t) -> Value ins (TOption t)
  VList      :: [Value ins t] -> Value ins (TList t)
  VSet       :: Set (CValue t) -> Value ins (TSet t)
  VOp        :: Operation ins -> Value ins TOperation
  VContract  :: Address -> Value ins (TContract p)
  VPair      :: (Value ins l, Value ins r) -> Value ins (TPair l r)
  VOr        :: Either (Value ins l) (Value ins r) -> Value ins (TOr l r)
  VLam       :: Show (ins '[inp] '[out])
             => ins (inp ': '[]) (out ': '[]) -> Value ins (TLambda inp out)
  VMap       :: Map (CValue k) (Value ins v) -> Value ins (TMap k v)
  VBigMap    :: Map (CValue k) (Value ins v) -> Value ins (TBigMap k v)

deriving instance Show (Value instr t)

-- TODO: actually we should handle big maps with something close
-- to following:
--
--  VBigMap :: BigMap op ref k v -> Val cp ('T_big_map k v)
--
-- data ValueOp v
--     = New v
--     | Upd v
--     | Rem
--     | NotExisted
--
-- data BigMap op ref k v = BigMap
--  { bmRef :: ref k v, bmChanges :: Map (CVal k) (ValueOp (Val cp v)) }

-- | Converts a complex Haskell structure into @Val@ representation and back
class Val a where
  toVal :: a -> Value instr (ToT a)
  fromVal :: Value instr (ToT a) -> a

instance Val Integer where
  toVal = VC . toCVal
  fromVal (VC cval) = fromCVal cval

instance Val Natural where
  toVal = VC . toCVal
  fromVal (VC cval) = fromCVal cval

instance Val Text where
  toVal = VC . toCVal
  fromVal (VC cval) = fromCVal cval

instance Val Bool where
  toVal = VC . toCVal
  fromVal (VC cval) = fromCVal cval

instance Val ByteString where
  toVal = VC . toCVal
  fromVal (VC cval) = fromCVal cval

instance Val Mutez where
  toVal = VC . toCVal
  fromVal (VC cval) = fromCVal cval

instance Val KeyHash where
  toVal = VC . toCVal
  fromVal (VC cval) = fromCVal cval

instance Val Timestamp where
  toVal = VC . toCVal
  fromVal (VC cval) = fromCVal cval

instance Val Address where
  toVal = VC . toCVal
  fromVal (VC cval) = fromCVal cval

instance Val () where
  toVal () = VUnit
  fromVal VUnit = ()

instance Val a => Val (Maybe a) where
  toVal Nothing = VOption Nothing
  toVal (Just a) = VOption (Just $ toVal a)
  fromVal (VOption Nothing) = Nothing
  fromVal (VOption (Just val)) = Just $ fromVal val

instance (Val a, Val b) => Val (Either a b) where
  toVal (Left l) = VOr $ Left $ toVal l
  toVal (Right r) = VOr $ Right $ toVal r
  fromVal (VOr (Left l)) = Left $ fromVal l
  fromVal (VOr (Right r)) = Right $ fromVal r

instance (Val a, Val b) => Val (a, b) where
  toVal (l, r) = VPair (toVal l, toVal r)
  fromVal (VPair (a, b)) = (fromVal a, fromVal b)

instance Val a => Val [a] where
  toVal = VList . map toVal
  fromVal (VList lVals) = map fromVal lVals

instance CVal k => Val (Set k) where
  toVal = VSet . Set.map toCVal
  fromVal (VSet s) = Set.map fromCVal s

instance (CVal k, Val a) => Val (Map k a) where
  toVal = VMap . Map.mapKeys toCVal . Map.map toVal
  fromVal (VMap m) = Map.map fromVal $ Map.mapKeys fromCVal m
