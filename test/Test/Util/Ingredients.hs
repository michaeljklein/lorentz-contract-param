-- | Ingridients that we use in our test suite.

module Test.Util.Ingredients
  ( ourIngredients
  ) where

import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Ingredients.Basic (listingTests)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Test.Tasty.Ingredients.FailFast (failFast)

-- | This is the default set of ingredients extended with the
-- 'failFast' decorator which enables the @--fast-fast@ option.
ourIngredients :: [Ingredient]
ourIngredients = [failFast consoleTestReporter, listingTests]
