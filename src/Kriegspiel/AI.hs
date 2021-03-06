{-# LANGUAGE DeriveGeneric #-}
{- |
   Module      : Kriegspiel.AI
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Basic AI opponent, that cannot win in practice but should offer some resistance.
-}

module Kriegspiel.AI (
  AI(faction),
  Turn(..),
  getState,
  mkAI,
  initialAI,
  playAI
  ) where

import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON, FromJSON )
import Data.Maybe ( fromJust )
import qualified Data.Map as M
import qualified Data.Set as S
import Kriegspiel.Game.Attack
import Kriegspiel.Game.GameState
import Kriegspiel.Game.Movement
import Kriegspiel.Game.Placement

-- | A parametrized AI.
data AI = AI { faction :: Faction,
               alpha :: Double,
               alpha' :: Double
             }
  deriving Generic

instance ToJSON AI
instance FromJSON AI

-- | Make a new parametrized AI.
mkAI :: Faction -> AI
mkAI me = AI { faction = me,
               alpha = 1,
               alpha' = 1
             } -- FIXME: parametrize beta coefficients.

beta :: AI -> Maybe (Unit, Faction) -> Double
beta _ Nothing = 1
beta ai (Just (u, f)) = if f == faction ai
  then 0.5
  else case u of
         Supplier -> 4
         MountedSupplier -> 5
         Infantry -> 1
         Cavalry -> 2
         Artillery -> 3
         MountedArtillery -> 3.5

beta' :: AI -> Maybe (Unit, Faction) -> Double
beta' _ Nothing = 1
beta' ai (Just (u, f)) = if f == faction ai
  then case u of
         Supplier -> -4
         MountedSupplier -> -5
         Infantry -> -1
         Cavalry -> -2
         Artillery -> -3
         MountedArtillery -> -3.5
  else -0.5

-- | One action taken by the AI.
data Turn = Mov Position Position GameState
          | Att Position GameState
          | Pas GameState
  deriving Generic

instance FromJSON Turn
instance ToJSON Turn

-- | Get the 'GameState' from a 'Turn'.
getState :: Turn -> GameState
getState (Mov _ _ gs) = gs
getState (Att _ gs) = gs
getState (Pas gs) = gs

-- | Let the AI place its units at the beginning of the game.
initialAI :: AI -> Placing
initialAI ai = Placing (faction ai) M.empty b
  where
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty units
    p_ :: Int -> Int -> Position -- Unsafe `mkPosition`
    p_ x y = fromJust $ mkPosition x y
    units = case (faction ai) of
      North -> [(p_ 21 2, Supplier, North),
                (p_ 18 2, MountedSupplier, North),
                (p_ 13 9, Artillery, North),
                (p_ 15 10, MountedArtillery, North),
                (p_ 20 9, Cavalry, North),
                (p_ 21 9, Cavalry, North),
                (p_ 17 10, Cavalry, North),
                (p_ 18 10, Cavalry, North),
                (p_ 8 4, Infantry, North),
                (p_ 21 8, Infantry, North),
                (p_ 12 10, Infantry, North),
                (p_ 13 10, Infantry, North),
                (p_ 14 10, Infantry, North),
                (p_ 16 10, Infantry, North),
                (p_ 11 10, Infantry, North),
                (p_ 11 9, Infantry, North),
                (p_ 12 9, Infantry, North)]
      South -> [(p_ 8 20, Supplier, South),
                (p_ 3 12, MountedSupplier, South),
                (p_ 9 12, Artillery, South),
                (p_ 8 12, MountedArtillery, South),
                (p_ 7 11, Cavalry, South),
                (p_ 7 12, Cavalry, South),
                (p_ 7 13, Cavalry, South),
                (p_ 8 11, Cavalry, South),
                (p_ 3 13 , Infantry, South),
                (p_ 10 11, Infantry, South),
                (p_ 10 12, Infantry, South),
                (p_ 10 13, Infantry, South),
                (p_ 10 14, Infantry, South),
                (p_ 9 11, Infantry, South),
                (p_ 8 13, Infantry, South),
                (p_ 8 14, Infantry, South),
                (p_ 8 15, Infantry, South)]

-- | Let the AI play one move.
playAI :: AI -> GameState -> Turn
playAI ai (GS p b) = if player p == faction ai
  then case p of
         Retreating _ -> movementsAI ai (movements (GS p b))
         Moving _ -> movementsAI ai (movements (GS p b))
         Attacking _ -> attacksAI ai (attacks (GS p b))
         Victory _ -> Pas (GS p b)
  else Pas (GS p b)

-- | Choose the best (according to the AI) movement available.
movementsAI :: AI -> Movements -> Turn
movementsAI _ (None gs) = Pas gs
movementsAI ai (Mandatory p mpg) =
  case takeBest (\ (GS ph b) -> rate ai (shaken ph) b) mpg of
    Nothing -> undefined -- Should not happen in practice
    Just (p', _, gs) -> Mov p p' gs
movementsAI ai (Optional (GS p b) mppg) =
  case takeBest (\ (GS p' b') -> rate ai (shaken p') b') (flatten mppg) of
    Nothing -> Pas (GS p b)
    Just ((p1, p2), best, gs') -> if best > rate ai (shaken p) b
                                  then Mov p1 p2 gs'
                                  else Pas (GS p b)

-- | Choose the best (according to the AI) attack available.
attacksAI :: AI -> Attacks -> Turn
attacksAI _ (Nil gs) = Pas gs
attacksAI ai (Attacks (GS p b) mpg) =
  case takeBest (\ (_, GS p' b') -> rate ai (shaken p') b') mpg of
    Nothing -> Pas (GS p b)
    Just (p0, best, (_, gs)) -> if best > rate ai (shaken p) b
                                then Att p0 gs
                                else Pas (GS p b)

-- | Flatten a map of maps.
flatten :: (Ord k1, Ord k2) => M.Map k1 (M.Map k2 a) -> M.Map (k1, k2) a
flatten = M.foldrWithKey go M.empty
  where
    go k v acc = M.union acc v'
      where
        v' = M.mapKeys (\ k' -> (k, k')) v

-- | Take the best element of a map, according to a given measure.
takeBest :: Ord k => (a -> Double) -> M.Map k a -> Maybe (k, Double, a)
takeBest f m = M.foldrWithKey go Nothing m
  where
    go k v Nothing = Just (k, f v, v)
    go k v (Just (k', x', v')) = if f v > x'
                                 then Just (k, f v, v)
                                 else Just (k', x', v')

-- | Rate the given board: a higher score means a better position for the AI.
rate :: AI -> Maybe Position -> Board -> Double
rate ai mp b = sum (S.map go whole)
  where
    go :: Position -> Double
    go p = (tanh $ (alpha ai) * (off - def')) * c / (1 + d')
           + (tanh $ (alpha' ai) * (off' - def)) * c' / (1 + d)
      where
        me = faction ai
        you = other me
        off = fromIntegral (offense b me mp p) :: Double
        off' = fromIntegral (offense b you mp p) :: Double
        def = fromIntegral (defence b me p) :: Double
        def' = fromIntegral (defence b you p) :: Double
        c = beta ai (funit b p)
        c' = beta' ai (funit b p)
        d = case spositions b me of
              Zero -> 0
              One p1 -> fromIntegral (dist p p1)
              Two p1 p2 -> fromIntegral ((dist p p1) + (dist p p2))
        d' = case spositions b you of
               Zero -> 0
               One p1 -> fromIntegral (dist p p1)
               Two p1 p2 -> fromIntegral ((dist p p1) + (dist p p2))
