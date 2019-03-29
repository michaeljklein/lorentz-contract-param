module Michelson.Printer
  ( RenderDoc(..)
  , printDoc
  , printUntypedContract
  ) where

import qualified Data.Text.Lazy as TL
import qualified Michelson.Untyped as Un
import Michelson.Printer.Util (printDoc, RenderDoc(..))

printUntypedContract :: (RenderDoc op) => Un.Contract op -> Text
printUntypedContract = TL.toStrict . printDoc . renderDoc
