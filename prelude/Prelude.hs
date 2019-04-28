{-
 - © 2019 Tocqueville Group
 -
 - SPDX-License-Identifier: AGPL-3.0-or-later
 -}

-- | This module essentially replaces the default Prelude with Universum.
--
-- It works because we are using the 'base-noprelude' package instead of 'base'.

module Prelude
       ( module Universum
       ) where

import Universum hiding (Key, Type, Val)
