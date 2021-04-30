{- |
   Module      : Kriegspiel.Game.Placement
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Admissible placements depending on the game state.
-}

module Kriegspiel.Game.Placement (
  initial,
  placements,
  merge
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Kriegspiel.Game.GameState
import Kriegspiel.Game.Utils

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
placements :: GameState -> M.Map Position (M.Map Unit GameState)
placements (GS phase b) = case phase of
  Placing pl -> fromKeys go1 ps
    where
      ps = S.filter (free b) (half (pplayer pl))
      go1 :: Position -> M.Map Unit GameState
      go1 p = M.mapWithKey go2 (ptodo pl)
        where
          go2 :: Unit -> Int -> GameState
          go2 u n = GS pl' b'
            where
              todo = if n > 1
                     then M.insert u (n - 1) (ptodo pl)
                     else M.delete u (ptodo pl)
              pl' = Placing $ Placing' { pplayer = pplayer pl,
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
