{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Untyped Michelson values (i. e. type of a value is not statically known).

module Michelson.Untyped.Value
  ( Value (..)
  , Elt (..)
  -- Internal types to avoid orphan instances
  , InternalByteString(..)
  , unInternalByteString
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import qualified Data.List as L
import qualified Data.Text.Lazy as LT
import Formatting.Buildable (Buildable(build))
import Text.Hex (encodeHex)
import Text.PrettyPrint.Leijen.Text (braces, dquotes, parens, semi, text, (<+>), (<++>))

import Michelson.Printer.Util (RenderDoc(..), printDoc, renderOps)

data Value op =
    ValueInt     Integer
  | ValueString  Text
  | ValueBytes   InternalByteString
  | ValueUnit
  | ValueTrue
  | ValueFalse
  | ValuePair    (Value op) (Value op)
  | ValueLeft    (Value op)
  | ValueRight   (Value op)
  | ValueSome    (Value op)
  | ValueNone
  | ValueNil
  | ValueSeq     (NonEmpty $ Value op)
  -- ^ A sequence of elements: can be a list or a set.
  -- We can't distinguish lists and sets during parsing.
  | ValueMap     (NonEmpty $ Elt op)
  | ValueLambda  (NonEmpty op)
  deriving stock (Eq, Show, Functor, Data, Generic)

data Elt op = Elt (Value op) (Value op)
  deriving stock (Eq, Show, Functor, Data, Generic)

-- | ByteString does not have an instance for ToJSON and FromJSON, to
-- avoid orphan type class instances, make a new type wrapper around it.
newtype InternalByteString = InternalByteString ByteString
  deriving stock (Data, Eq, Show)

unInternalByteString :: InternalByteString -> ByteString
unInternalByteString (InternalByteString bs) = bs

instance RenderDoc op => RenderDoc (Value op) where
  renderDoc =
    \case
      ValueNil       -> mempty
      ValueInt x     -> text . LT.pack . show $ x 
      ValueString x  -> dquotes (text . LT.fromStrict $ x)
      ValueBytes xs  -> "0x" <+> (text . LT.fromStrict . encodeHex . unInternalByteString $ xs)
      ValueUnit      -> "Unit"
      ValueTrue      -> "True"
      ValueFalse     -> "False"
      ValuePair l r  -> parens $ ("Pair"  <++> renderDoc l <++> renderDoc r)
      ValueLeft l    -> parens $ ("Left"  <++> renderDoc l)
      ValueRight r   -> parens $ ("Right" <++> renderDoc r)
      ValueSome x    -> parens $ ("Some"  <++> renderDoc x)
      ValueNone      -> "None"
      ValueSeq xs    -> braces $ mconcat $ (L.intersperse semi (renderDoc <$> toList xs))
      ValueMap xs    -> braces $ mconcat $ (L.intersperse semi (renderDoc <$> toList xs))
      ValueLambda xs -> renderOps xs

instance RenderDoc op => RenderDoc (Elt op) where
  renderDoc (Elt k v) = "Elt" <+> renderDoc k <+> renderDoc v

instance (RenderDoc op) => Buildable (Value op) where
  build = build . printDoc . renderDoc

instance (RenderDoc op) => Buildable (Elt op) where
  build = build . printDoc . renderDoc

----------------------------------------------------------------------------
-- JSON serialization
----------------------------------------------------------------------------

-- it is not possible to derives these automatically because
-- ByteString does not have a ToJSON or FromJSON instance

instance ToJSON InternalByteString where
  toJSON = toJSON @Text . decodeUtf8 . unInternalByteString

instance FromJSON InternalByteString where
  parseJSON = fmap (InternalByteString . encodeUtf8 @Text) . parseJSON

deriveJSON defaultOptions ''Value
deriveJSON defaultOptions ''Elt
