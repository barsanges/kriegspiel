{- |
   Module      : Kriegspiel.Game.Placement
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Admissible placements depending on the game state.
-}

module Kriegspiel.Game.Placement (
  Placements,
  initial,
  placements,
  merge
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Kriegspiel.Game.GameState
import Kriegspiel.Game.Utils

type Placements = M.Map Position (Either GameState (M.Map Unit GameState))

-- | Initialise a new game.
initial :: Faction -> GameState
initial f = GS phase bempty
  where
    allunits = M.fromList [(Supplier, 1),
                           (MountedSupplier, 1),
                           (Infantry, 9),
                           (Cavalry, 4),
                           (Artillery, 1),
                           (MountedArtillery, 1)]
    phase = Placing $ Placing' { pplayer = f,
                                 ptodo = allunits
                               }

-- | All admissible placements for a given game state.
placements :: GameState -> Placements
placements (GS phase b) = case phase of
  Placing pl -> fromKeys go ps
    where
      me = pplayer pl
      ps = S.filter (\ p -> tile p /= Mountain) (half me)

      go :: Position -> Either GameState (M.Map Unit GameState)
      go p = case unit b p of
        Just u -> Left (undo p u)
        Nothing -> Right (put p)

      undo :: Position -> Unit -> GameState
      undo p u = GS pl' b'
        where
          todo = M.insertWith (+) u 1 (ptodo pl)
          pl' = Placing $ Placing' { pplayer = me,
                                     ptodo = todo
                                   }
          b' = rm b p

      put :: Position -> M.Map Unit GameState
      put p = M.mapWithKey h (ptodo pl)
        where
          h :: Unit -> Int -> GameState
          h u n = GS pl' b'
            where
              todo = if n > 1
                     then M.insert u (n - 1) (ptodo pl)
                     else M.delete u (ptodo pl)
              pl' = Placing $ Placing' { pplayer = me,
                                         ptodo = todo
                                       }
              b' = add' b p u (pplayer pl)

  _ -> M.empty

-- | Merge two game states corresponding to the placement of all units from
-- both North and South.
merge :: GameState -> GameState -> Maybe GameState
merge (GS phase b) (GS phase' b') = case (phase, phase') of
  (Placing p, Placing p') -> if (pplayer p) /= (pplayer p') && (done p) && (done p')
    then Just (GS newphase newb)
    else Nothing
    where
      done x = (ptodo x == M.empty)
      newphase = Moving $ Moving' { nmoves = 0,
                                    mplayer = North, -- Differs from the specs
                                    mshaken = Nothing,
                                    moved = S.empty,
                                    attack = False
                                  }
      newb = foldr go b (upositions b' (pplayer p'))
      go :: Position -> Board -> Board
      go p0 b0 = case funit b' p0 of
        Nothing -> b0 -- Should not happen
        Just (u0, f0) -> add' b0 p0 u0 f0
  _ -> Nothing
