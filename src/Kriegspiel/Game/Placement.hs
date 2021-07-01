{- |
   Module      : Kriegspiel.Game.Placement
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Admissible placements depending on the game state.
-}

module Kriegspiel.Game.Placement (
  Placing(..),
  Placements,
  initial,
  placements,
  merge
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Kriegspiel.Game.GameState
import Kriegspiel.Game.Utils

type Placements = M.Map Position (Either Placing (M.Map Unit Placing))

-- | Informations related to a "Placing" phase.
data Placing = Placing Faction (M.Map Unit Int) Board
  deriving (Eq, Show)

-- | Initialise a new game.
initial :: Faction -> Placing
initial f = Placing f allunits bempty
  where
    allunits = M.fromList [(Supplier, 1),
                           (MountedSupplier, 1),
                           (Infantry, 9),
                           (Cavalry, 4),
                           (Artillery, 1),
                           (MountedArtillery, 1)]

-- | All admissible placements for a given game state.
placements :: Placing -> Placements
placements (Placing me ptodo b) = fromKeys go ps
  where
    ps = S.filter (\ p -> tile p /= Mountain) (half me)

    go :: Position -> Either Placing (M.Map Unit Placing)
    go p = case unit b p of
      Just u -> Left (undo p u)
      Nothing -> Right (put p)

    undo :: Position -> Unit -> Placing
    undo p u = Placing me todo b'
      where
        todo = M.insertWith (+) u 1 ptodo
        b' = rm b p

    put :: Position -> M.Map Unit Placing
    put p = M.mapWithKey h ptodo
      where
        h :: Unit -> Int -> Placing
        h u n = Placing me todo b'
          where
            todo = if n > 1
                   then M.insert u (n - 1) ptodo
                   else M.delete u ptodo
            b' = add' b p u me


-- | Merge two game states corresponding to the placement of all units from
-- both North and South.
merge :: Placing -> Placing -> Maybe GameState
merge (Placing f p b) (Placing f' p' b') = if f /= f' && (p == M.empty) && (p' == M.empty)
                                           then Just (GS newphase newb)
                                           else Nothing
    where
      newphase = Moving $ Moving' { nmoves = 0,
                                    mplayer = North, -- Differs from the specs
                                    mshaken = Nothing,
                                    moved = S.empty,
                                    attack = False
                                  }
      newb = foldr go b (upositions b' f')
      go :: Position -> Board -> Board
      go p0 b0 = case funit b' p0 of
        Nothing -> b0 -- Should not happen
        Just (u0, f0) -> add' b0 p0 u0 f0
