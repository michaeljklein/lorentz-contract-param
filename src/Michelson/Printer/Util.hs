module Michelson.Printer.Util
  ( RenderDoc(..)
  , printDoc
  , renderOps
  , renderOpsList
  , spaces
  , wrapInParens
  , buildRenderDoc
  ) where

import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (Builder)
import Text.PrettyPrint.Leijen.Text
  (Doc, SimpleDoc, braces, cat, displayB, displayT, hcat, isEmpty, parens, punctuate, renderPretty,
  semi, space, (<++>))

-- | Generalize converting a type into a
-- Text.PrettyPrint.Leijen.Text.Doc. Used to pretty print Michelson code
-- and define Fmt.Buildable instances.
class RenderDoc a where
  renderDoc :: a -> Doc

-- | Convert 'Doc' to 'Text' with a line width of 80.
printDoc :: Doc -> LT.Text
printDoc = displayT . doRender

-- | Generic way to render the different op types that get passed
-- to a contract.
renderOps :: (RenderDoc op) => NonEmpty op -> Doc
renderOps = renderOpsList . toList

renderOpsList :: (RenderDoc op) => [op] -> Doc
renderOpsList ops = braces $ cat $ punctuate semi (renderDoc <$> ops)

-- | Create a specific number of spaces.
spaces :: Int -> Doc
spaces x = hcat $ replicate x space

-- | Wrap documents in parentheses if there are two or more in the list.
wrapInParens :: NonEmpty Doc -> Doc
wrapInParens ds =
  if (length $ filter (not . isEmpty) (toList ds)) > 1
    then parens $ foldr (<++>) mempty ds
    else foldr (<++>) mempty ds

-- | Turn something that is instance of `RenderDoc` into a `Builder`.
-- It's formatted the same way as `printDoc` formats docs.
buildRenderDoc :: RenderDoc a => a -> Builder
buildRenderDoc = displayB . doRender . renderDoc

doRender :: Doc -> SimpleDoc
doRender = renderPretty 0.4 80
