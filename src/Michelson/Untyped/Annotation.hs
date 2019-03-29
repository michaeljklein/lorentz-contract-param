{-# LANGUAGE DeriveDataTypeable, DerivingStrategies #-}

-- | Michelson annotations in untyped model.

module Michelson.Untyped.Annotation
  ( Annotation (..)
  , pattern WithAnn
  , TypeAnn
  , FieldAnn
  , VarAnn
  , noAnn
  , ann
  , unifyAnn
  , ifAnnUnified
  , disjoinVn
  , convAnn
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Data (Data(..))
import Data.Default (Default(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Fmt (Buildable(build))
import qualified Text.Show
import Text.PrettyPrint.Leijen.Text (text)

import Michelson.Printer.Util (RenderDoc(..), printDoc)

newtype Annotation tag = Annotation T.Text
  deriving stock (Eq, Data, Functor, Generic)
  deriving newtype (IsString)

instance Default (Annotation tag) where
  def = Annotation ""

instance Show (Annotation FieldTag) where
  show (Annotation x) = "%" <> toString x

instance Show (Annotation VarTag) where
  show (Annotation x) = "@" <> toString x

instance Show (Annotation TypeTag) where
  show (Annotation x) = ":" <> toString x

data TypeTag
data FieldTag
data VarTag

type TypeAnn = Annotation TypeTag
type FieldAnn = Annotation FieldTag
type VarAnn = Annotation VarTag


instance RenderDoc TypeAnn where
  renderDoc (Annotation "") = ""
  renderDoc (Annotation a)  = ":" <> (text . LT.fromStrict $ a)

instance RenderDoc FieldAnn where
  renderDoc (Annotation "") = ""
  renderDoc (Annotation a)  = "%" <> (text . LT.fromStrict $ a)

instance RenderDoc VarAnn where
  renderDoc (Annotation "") = ""
  renderDoc (Annotation a)  = "@" <> (text . LT.fromStrict $ a)

instance Buildable TypeAnn where
  build = build . printDoc . renderDoc

instance Buildable FieldAnn where
  build = build . printDoc . renderDoc

instance Buildable VarAnn where
  build = build . printDoc . renderDoc

noAnn :: Annotation a
noAnn = Annotation ""

ann :: T.Text -> Annotation a
ann = Annotation

instance Semigroup VarAnn where
  Annotation a <> Annotation b
    | a == "" || b == "" = ann $ a <> b
    | otherwise          = ann $ a <> "." <> b

instance Monoid VarAnn where
    mempty = noAnn

unifyAnn :: Annotation tag -> Annotation tag -> Maybe (Annotation tag)
unifyAnn (Annotation ann1) (Annotation ann2)
  | ann1 == "" || ann2 == "" = Just $ ann $ ann1 <> ann2
  | ann1 == ann2 = Just $ ann ann1
  | otherwise  = Nothing

ifAnnUnified :: Annotation tag -> Annotation tag -> Bool
ifAnnUnified a1 a2 = isJust $ a1 `unifyAnn` a2

disjoinVn :: VarAnn -> (VarAnn, VarAnn)
disjoinVn (Annotation a) = case T.findIndex (== '.') $ T.reverse a of
  Just ((n - 1 -) -> pos) -> (ann $ T.take pos a, ann $ T.drop (pos + 1) a)
  Nothing                 -> (noAnn, ann a)
  where
    n = T.length a

convAnn :: Annotation tag1 -> Annotation tag2
convAnn (Annotation a) = Annotation a

pattern WithAnn :: Annotation tag -> Annotation tag
pattern WithAnn ann <- ann@(Annotation (toString -> _:_))

deriveJSON defaultOptions ''Annotation
