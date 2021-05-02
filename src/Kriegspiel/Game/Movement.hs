{- |
   Module      : Kriegspiel.Game.Movement
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Admissible movements depending on the game state.
-}

module Kriegspiel.Game.Movement (
  Movements(..),
  movements
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Kriegspiel.Game.GameState
import Kriegspiel.Game.Utils

-- | All admissible movements for a given game state.
data Movements = None
               | Mandatory Position (M.Map Position GameState)
               | Optional GameState (M.Map Position (M.Map Position GameState))

-- | Is it allowed to destroy a store during the movement?
data StoreDestruction = Allowed | Forbidden
  deriving Eq

-- | Build all admissible movements for a given game state.
movements :: GameState -> Movements
movements (GS phase b) = case phase of
  Retreating r -> retreat b r
  Moving m -> move b m
  _ -> None

-- | Build the phase following a retreat.
pretreat :: Retreating' -> Position -> StoreDiff -> Phase
pretreat r p _ = Moving $ Moving' { nmoves = 1,
                                    mplayer = rplayer r,
                                    mshaken = Just p,
                                    moved = S.singleton p,
                                    attack = False
                                  }

-- | Build the phase following a move.
pmove :: Moving' -> Position -> StoreDiff -> Phase
pmove m p sd
  | sd == Second = Victory (mplayer m)
  | nmoves m < 4 = Moving $ Moving' { nmoves = 1 + nmoves m,
                                      mplayer = mplayer m,
                                      mshaken = mshaken m,
                                      moved = S.insert p (moved m),
                                      attack = (sd == First) || (attack m)
                                    }
  | otherwise = pend m

-- | Build the phase after the end of the current player's movement phase.
pend :: Moving' -> Phase
pend m = if attack m
         then Moving $ Moving' { nmoves = 0,
                                 mplayer = other (mplayer m),
                                 mshaken = Nothing,
                                 moved = S.empty,
                                 attack = False
                               }
         else Attacking $ Attacking' { aplayer = mplayer m,
                                       ashaken = mshaken m
                                     }

-- | Get all admissible movements for a retreat.
retreat :: Board -> Retreating' -> Movements
retreat b r = Mandatory x $ enumerate b (rplayer r) Forbidden (pretreat r) x
  where
    x = rshaken r

-- | Get all regular admissible movements.
move :: Board -> Moving' -> Movements
move b m = Optional p ms
  where
    p = pass b m
    ms = fromKeys (enumerate b (mplayer m) a (pmove m)) (movable b m)
    a = if attack m
        then Forbidden
        else Allowed

-- | Pass: stop all movements and start attacking.
pass :: Board -> Moving' -> GameState
pass b m = GS (pend m) b

-- | Get all units that are allowed to move.
movable :: Board -> Moving' -> S.Set Position
movable b m = S.difference (upositions b (mplayer m)) (moved m)

-- | Enumerate all possible movements starting from a given position.
enumerate :: Board
          -> Faction
          -> StoreDestruction
          -> (Position -> StoreDiff -> Phase)
          -> Position
          -> M.Map Position GameState
enumerate b f a g p = fromKeys go (reachable b f a p)
  where
    go :: Position -> GameState
    go p' = let (sd, b') = mv b p p'
            in GS (g p' sd) b'

-- | Get all positions that are reachable for a unit at a given position.
reachable :: Board -> Faction -> StoreDestruction -> Position -> S.Set Position
reachable b f a p = case unit b p of
  Nothing -> S.empty
  Just u -> S.filter go (circle p (speed u))
    where
      go = if (a == Allowed) || (not $ military u)
           then tests [free b, hasPath b f p]
           else tests [free b, hasPath b f p, noAttack b f p]

-- | Test if there is a path between two positions.
hasPath :: Board -> Faction -> Position -> Position -> Bool
hasPath b f p p'
  | d == 1 = True
  | d == 2 = any (supplied b f) (S.intersection ps ps')
  | otherwise = False
  where
    d = dist p p'
    ps = neighbors p
    ps' = neighbors p'

-- | Test if a movement is an attack ('False') or not ('True').
noAttack :: Board -> Faction -> Position -> Position -> Bool
noAttack b f p p' = case unit b p of
  Just Supplier -> True
  Just MountedSupplier -> True
  _ -> case spositions b (other f) of
    Zero -> True
    One q -> p' /= q
    Two q1 q2 -> (p' /= q1) && (p' /= q2)
