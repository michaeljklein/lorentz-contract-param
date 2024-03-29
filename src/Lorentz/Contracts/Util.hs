
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists -Wno-missing-monadfail-instances #-}

module Lorentz.Contracts.Util where

import Data.Char
import Data.Functor.Classes
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prelude hiding (readEither, unlines, unwords)
import Text.ParserCombinators.ReadP (ReadP)
import Text.Read
import qualified Text.ParserCombinators.ReadP as P

import Data.Aeson
import Data.Functor.Contravariant
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Text as T

import Lorentz.Contracts.UnsafeLedger
import Lorentz.Contracts.Walker
import Lorentz.Macro
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Instr (Instr)
import Michelson.Typed.Value
import Named
import Tezos.Address
import Tezos.Crypto
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Lorentz.Contracts.ManagedLedger.Athens as Athens
import qualified Lorentz.Contracts.ManagedLedger.Babylon as Babylon
import qualified Lorentz.Contracts.ManagedLedger.Proxy as Proxy
import qualified Lorentz.Contracts.UnsafeLedger as UnsafeLedger
import qualified Lorentz.Contracts.Walker as Walker


deriving instance Read KeyHash
deriving instance Read Babylon.Parameter
deriving instance Read UnsafeLedger.Parameter
deriving instance Read Walker.Parameter

deriving instance Read Proxy.Parameter
deriving instance Read Proxy.Parameter0
deriving instance Read Proxy.Parameter1
deriving instance Read Proxy.Parameter2

instance IsoValue (Value' Instr t) where
  type ToT (Value' Instr t) = t
  toVal = id
  fromVal = id

instance IsoCValue (CValue t) where
  type ToCT (CValue t) = t
  toCVal = id
  fromCVal = id


-- | Parse something between the two given `Char`'s
betweenChars :: Char -> Char -> ReadP a -> ReadP a
betweenChars beforeChar afterChar =
  P.char beforeChar `P.between` P.char afterChar

-- | Parse something in parentheses
inParensP :: ReadP a -> ReadP a
inParensP = '(' `betweenChars` ')'

-- | Parse something in double-quotes: @"[something]"@
inQuotesP :: ReadP a -> ReadP a
inQuotesP = '"' `betweenChars` '"'

-- | Attempt to parse with given modifier, otherwise parse without
maybeLiftP :: (ReadP a -> ReadP a) -> ReadP a -> ReadP a
maybeLiftP liftP = liftM2 (<|>) liftP id

-- | Attempt to parse `inParensP`, else parse without
maybeInParensP :: ReadP a -> ReadP a
maybeInParensP = maybeLiftP inParensP

-- | Attempt to parse `inQuotesP`, else parse without
maybeInQuotesP :: ReadP a -> ReadP a
maybeInQuotesP = maybeLiftP inQuotesP

-- | Read an `Address`, inside or outside of @""@'s
readAddressP :: ReadP Address
readAddressP =
      maybeInParensP . maybeInQuotesP $ do
        ensureAddressPrefix
        addressStr <- P.munch1 isAlphaNum
        case parseAddress $ T.pack addressStr of
          Left err -> fail $ show err
          Right address' -> return address'
  where
    ensureAddressPrefix =
      (do {('t':'z':'1':_) <- P.look; return ()}) <|>
      (do {('K':'T':'1':_) <- P.look; return ()})

instance Read Athens.Parameter where
  readPrec =
    choice
      [ readUnaryWith (parens readPrec) "Transfer" Athens.Transfer
      , readUnaryWith (parens readPrec) "TransferViaProxy" Athens.TransferViaProxy
      , readUnaryWith (parens readPrec) "Approve" Athens.Approve
      , readUnaryWith (parens readPrec) "ApproveViaProxy" Athens.ApproveViaProxy
      , readUnaryWith (parens readPrec) "GetAllowance" Athens.GetAllowance
      , readUnaryWith (parens readPrec) "GetBalance" Athens.GetBalance
      , readUnaryWith (parens readPrec) "GetTotalSupply" Athens.GetTotalSupply
      , readUnaryWith (parens readPrec) "SetPause" Athens.SetPause
      , readUnaryWith (parens readPrec) "SetAdministrator" Athens.SetAdministrator
      , readUnaryWith (parens readPrec) "GetAdministrator" Athens.GetAdministrator
      , readUnaryWith (parens readPrec) "Mint" Athens.Mint
      , readUnaryWith (parens readPrec) "Burn" Athens.Burn
      , readUnaryWith (parens readPrec) "SetProxy" Athens.SetProxy
      ]

instance Read Address where
  readPrec = readP_to_Prec $ const readAddressP

instance Read (ContractAddr cp) where
  readPrec =
    readP_to_Prec . const $ do
      P.string "ContractAddr"
      P.skipSpaces
      ContractAddr <$> readAddressP

instance Read a => Read (View a r) where
  readPrec =
    readP_to_Prec $ \prec' -> do
      P.skipSpaces
      P.string "View"
      P.skipSpaces
      viewArg <- readPrec_to_P (parens readPrec) prec'
      P.skipSpaces
      View viewArg . ContractAddr <$> readAddressP

instance (Read a, KnownSymbol name) => Read (NamedF Identity a name) where
  -- show (ArgF a) = symbolVal (Proxy @name) <> " :! " <> show a
  readPrec = readPrec' Proxy
    where
      readPrec' ::
           (Read a', KnownSymbol name')
        => proxy name'
        -> ReadPrec (NamedF Identity a' name')
      readPrec' nameProxy =
        readP_to_Prec $ \prec' -> do
          P.skipSpaces
          maybeInQuotesP . P.string $ symbolVal nameProxy
          P.skipSpaces
          P.string ".!"
          P.skipSpaces
          ArgF <$> readPrec_to_P readPrec prec'

instance Read PublicKey where
  readPrec = readP_to_Prec $ \_ ->
    maybeInQuotesP $ do
      eNonQuoteChars <- parsePublicKey . T.pack <$> P.munch1 isAlphaNum
      case eNonQuoteChars of
        Left err -> fail $ show err
        Right res -> return res

instance Read SecretKey where
  readPrec = readP_to_Prec $ \_ ->
    maybeInQuotesP $ do
      eNonQuoteChars <- parseSecretKey . T.pack <$> P.munch1 isAlphaNum
      case eNonQuoteChars of
        Left err -> fail $ show err
        Right res -> return res

instance Read Signature where
  readPrec = readP_to_Prec $ \_ ->
    maybeInQuotesP $ do
      eNonQuoteChars <- parseSignature . T.pack <$> P.munch1 isAlphaNum
      case eNonQuoteChars of
        Left err -> fail $ show err
        Right res -> return res

deriving instance Ord PublicKey

-- | Since `Ed25519.PublicKey` doesn't expose
-- many instances, we convert to `String` and
-- compare the results
instance Ord Ed25519.PublicKey where
  compare x y = show x `compare` (show y :: String)

instance ToJSON Ed25519.PublicKey where
  toJSON = Aeson.String . formatPublicKey . PublicKey
  toEncoding = Aeson.text . formatPublicKey . PublicKey

instance FromJSON Ed25519.PublicKey where
  parseJSON =
    Aeson.withText "PublicKey" $
    either (fail . show) (pure . unPublicKey) . parsePublicKey


instance ToJSONKey Ed25519.PublicKey where
instance FromJSONKey Ed25519.PublicKey where

instance ToJSONKey PublicKey where
  toJSONKey = contramap unPublicKey toJSONKey
  toJSONKeyList = contramap (fmap unPublicKey) toJSONKeyList

instance FromJSONKey PublicKey where
  fromJSONKey = fmap PublicKey fromJSONKey
  fromJSONKeyList = fmap (fmap PublicKey) fromJSONKeyList

