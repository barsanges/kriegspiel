{- |
   Module      : Kriegspiel.Game.Utils
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Useful functions.
-}

module Kriegspiel.Game.Utils (
  fromKeys,
  fromKeysMaybe,
  tests
  ) where

import qualified Data.Map as M

-- | Build a map from a set of keys.
fromKeys :: (Ord k, Foldable t) => (k -> a) -> t k -> M.Map k a
fromKeys f ks = foldr add M.empty ks
  where
    add k m = M.insert k (f k) m

-- | Build a map from a subset of a set of keys.
fromKeysMaybe :: (Ord k, Foldable t) => (k -> Maybe a) -> t k -> M.Map k a
fromKeysMaybe f ks = M.mapMaybe f (fromKeys id ks)

-- | Test if a value verifies several conditions.
tests :: [a -> Bool] -> a -> Bool
tests [] _ = True
tests (f:fs) x = f x && tests fs x
