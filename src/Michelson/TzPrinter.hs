module Michelson.Printer
  ( printUntypedContract
  ) where

import qualified Data.Text.Lazy as TL
import qualified Michelson.Untyped as Un

printUntypedContract :: (Un.RenderDoc op) => Un.Contract op -> Text
printUntypedContract = TL.toStrict . Un.printDoc . Un.renderDoc
