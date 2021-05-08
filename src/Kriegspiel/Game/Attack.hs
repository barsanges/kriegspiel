{- |
   Module      : Kriegspiel.Game.Attack
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Admissible attacks depending on the game state.
-}

module Kriegspiel.Game.Attack (
  Attacks(..),
  AttackResult(..),
  attacks
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Kriegspiel.Game.GameState
import Kriegspiel.Game.Utils

-- | Effect of an attack on the target.
data AttackResult = Defeated | Destroyed
  deriving (Eq, Show)

-- | All admissible attacks for a given "Attack" turn.
data Attacks = Nil GameState
             | Attacks GameState (M.Map Position (AttackResult, GameState))
  deriving (Eq, Show)

-- | Build all admissible attacks for a given game state.
attacks :: GameState -> Attacks
attacks (GS phase b) = case phase of
  Attacking a -> if M.null allowed
                 then if S.null $ upositions b (other (aplayer a))
                      then Nil (GS (Victory (aplayer a)) b)
                      else Nil pass
                 else Attacks pass allowed
    where
      pass = GS (pend a Nothing) b
      targets = upositions b (other (aplayer a))
      allowed = (fromKeysMaybe (resolve b a) targets)
  _ -> Nil (GS phase b)

-- | Build the phase after the end of the current player's turn.
pend :: Attacking' -> Maybe Position -> Phase
pend a mp = Moving $ Moving' { nmoves = 0,
                               mplayer = other (aplayer a),
                               mshaken = mp,
                               moved = S.empty,
                               attack = False
                             }

-- | Resolve an attack on a given position.
resolve :: Board -> Attacking' -> Position -> Maybe (AttackResult, GameState)
resolve b a p
  | off > def + 1 = destroyed
  | off == def + 1 = if (mayEscape b f' p)
    then defeated
    else destroyed
  | otherwise = Nothing
  where
    f = aplayer a
    f' = other f
    stakeholders = bstar p maxrange
    off = offense b a p stakeholders
    def = defence b f' p stakeholders
    defeated = Just (Defeated, GS (pend a (Just p)) b)
    b' = rm b p
    destroyed = if S.null (upositions b' f')
      then Just (Destroyed, GS (Victory f) b')
      else Just (Destroyed, GS (pend a Nothing) b')

-- | Compute the offensive strength of a faction at a given position.
offense :: Board -> Attacking' -> Position -> Star -> Int
offense b a p (Star s) = sum (fmap charge s)
  where
    f = aplayer a
    ready p' = (Just p') /= (ashaken a)

    charge :: [Position] -> Int
    charge [] = 0
    charge (p':ps) = case funit b p' of
      Nothing -> nocharge ps
      Just (u, f') -> if (f' == f) && (u == Cavalry) && (ready p') && (supplied b f p')
        then cstrength + (charge ps)
        else nocharge (p':ps)

    nocharge :: [Position] -> Int
    nocharge ps = sum (fmap go ps)
      where
        go :: Position -> Int
        go p' = case funit b p' of
          Nothing -> 0
          Just (u, f') -> if (f' == f) && (dist p p' <= range u) && (ready p') && (supplied b f p')
            then ostrength u
            else 0

-- | Compute the defensive strength of a faction at a given position.
defence :: Board -> Faction -> Position -> Star -> Int
defence b f p (Star s) = (go2 p) + sum (fmap go1 s)
  where
    go1 :: [Position] -> Int
    go1 ps = sum (fmap go2 ps)
    go2 :: Position -> Int
    go2 p' = case funit b p' of
      Nothing -> 0
      Just (u, f') -> if (f' == f) && (dist p p' <= range u) && (supplied b f p')
        then dstrength u (tile p')
        else 0

-- | Test if a unit may escape after having been defeated.
mayEscape :: Board -> Faction -> Position -> Bool
mayEscape b f p = (supplied b f p) && (any (free b) (neighbors p))
