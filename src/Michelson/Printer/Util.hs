module Michelson.Printer.Util
  ( RenderDoc(..)
  , printDoc
  , renderOps
  , renderOpsList
  , spaces
  , wrapInParens
  ) where

import qualified Data.Text.Lazy as LT
import Text.PrettyPrint.Leijen.Text
  (Doc, braces, cat, displayT, hcat, isEmpty, parens, punctuate, renderPretty, semi, space, (<++>))

-- | Generalize converting a type into a
-- Text.PrettyPrint.Leijen.Text.Doc. Used to pretty print Michelson code
-- and define Fmt.Buildable instances.
class RenderDoc a where
  renderDoc :: a -> Doc

-- | Convert 'Doc' to 'Text' with a line width of 80.
printDoc :: Doc -> LT.Text
printDoc = displayT . renderPretty 0.4 80

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
