module Michelson.Printer
  ( RenderDoc(..)
  , printDoc
  , printUntypedContract
  , printTypedContract
  , printSomeContract
  , printTypedValue
  ) where

import Data.Singletons (SingI)
import qualified Data.Text.Lazy as TL

import Michelson.Printer.Util (RenderDoc(..), printDoc)
import Michelson.TypeCheck.Types (HST(..), SomeContract(..))
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U
import Michelson.Typed.Annotation (Notes)

-- | Convert an untyped contract into a textual representation which
-- will be accepted by the OCaml reference client.
printUntypedContract :: (RenderDoc op) => Bool -> U.Contract' op -> TL.Text
printUntypedContract forceSingleLine = printDoc forceSingleLine . renderDoc

-- | Convert a typed contract into a textual representation which
-- will be accepted by the OCaml reference client.
printTypedContract :: (SingI p, SingI s) => Bool -> T.Contract p s -> TL.Text
printTypedContract forceSingleLine =
  printUntypedContract forceSingleLine . T.convertContract

printTypedContractWithNotes
  :: (SingI p, SingI s)
  => Bool -> (T.Contract p s, Notes (T.ContractInp1 p s)) -> TL.Text
printTypedContractWithNotes forceSingleLine =
  printUntypedContract forceSingleLine . T.convertContractWithNotes

printTypedValue :: (SingI t, T.HasNoOp t) => Bool -> T.Value t -> TL.Text
printTypedValue forceSingleLine = printDoc forceSingleLine . renderDoc . T.untypeValue

printSomeContract :: Bool -> SomeContract -> TL.Text
printSomeContract forceSingleLine (SomeContract c ((_, n,_) ::& _) _)
  = printTypedContractWithNotes forceSingleLine (c, n)
