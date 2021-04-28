{- |
   Module      : Kriegspiel.Game.Board
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

The board used in the game, with all its pieces.
-}

module Kriegspiel.Game.Board (
  module Kriegspiel.Game.Faction,
  module Kriegspiel.Game.Tile,
  module Kriegspiel.Game.Unit,
  Board,
  Position,
  Star(..),
  StoreDiff(..),
  Supply(..),
  width,
  height,
  bempty,
  half,
  northern,
  southern,
  mv,
  rm,
  add,
  add',
  dist,
  circle,
  neighbors,
  star,
  bstar,
  unit,
  funit,
  upositions,
  spositions,
  supplied,
  free,
  tile
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Kriegspiel.Game.Faction
import Kriegspiel.Game.Tile
import Kriegspiel.Game.Unit

-- | The board used in the game, with all its pieces.
data Board = B { pieces :: M.Map Position (Unit, Faction),
                 nstores :: Supply,
                 nsupply :: S.Set Position,
                 sstores :: Supply,
                 ssupply :: S.Set Position
               }

-- | A position on the board.
data Position = P Int Int
  deriving (Eq, Ord)

-- | 8 lines of positions (one for each direction). In each line, the first
-- element of the list is the closest to the center of the star. The center
-- itself is not included.
newtype Star = Star [[Position]]

-- | Indicates if an ennemy store has been destroyed.
data StoreDiff = Same | First | Second
  deriving Eq

-- | Position of the supply sources of a faction.
data Supply = Zero
            | One Position
            | Two Position Position
  deriving Eq

-- | The width of the board.
width :: Int
width = 25

-- | The height of the board.
height :: Int
height = 20

-- | Get all the positions supplied for a given faction.
resupply :: Supply -- ^ Stores
         -> Supply -- ^ Suppliers
         -> S.Set Position -- ^ Military units
         -> S.Set Position -- ^ Opponents
         -> S.Set Position -- ^ Supplied positions
resupply st su allied opponents = res
  where
    fromstores = case st of
      Zero -> S.empty
      One p -> direct p opponents
      Two p q -> S.union (direct p opponents) (direct q opponents)
    fromsuppliers = case su of
      Zero -> fromstores
      One p -> if S.member p fromstores
               then S.union fromstores (direct p opponents)
               else fromstores
      Two p q -> if S.member p fromstores
                 then let tmp = S.union fromstores (direct p opponents)
                      in if S.member q tmp
                         then S.union tmp (direct q opponents)
                         else tmp
                 else if S.member q fromstores
                      then let tmp = S.union fromstores (direct q opponents)
                           in if S.member p tmp
                              then S.union tmp (direct p opponents)
                              else tmp
                      else fromstores
    res = S.difference (indirect fromsuppliers allied) opponents

-- | Get all the positions directly supplied by a given position (assuming
-- this is a store or a supplier).
direct :: Position -> S.Set Position -> S.Set Position
direct center opponents = foldr S.union (S.singleton center) (fmap go pss)
  where
    (Star pss) = star center
    go :: [Position] -> S.Set Position
    go [] = S.empty
    go (p:ps) = if S.member p opponents
                then S.empty
                else S.insert p (go ps)

-- | Get all the positions indirectly supplied.
indirect :: S.Set Position -> S.Set Position -> S.Set Position
indirect sup allied = if (S.size sup') > (S.size sup)
                      then indirect sup' (S.difference allied sup)
                      else sup'
  where
    sup' = S.filter go2 $ foldr go1 sup (S.intersection sup allied)

    go1 :: Position -> S.Set Position -> S.Set Position
    go1 x ys = S.union ys (neighbors x)

    go2 :: Position -> Bool
    go2 p = tile p /= Mountain

-- | Make a board based on fundamental elements.
mk :: M.Map Position (Unit, Faction) -> Supply -> Supply -> Board
mk pieces_ nstores_ sstores_ = B { pieces = pieces_,
                                   nstores = nstores_,
                                   nsupply = ns,
                                   sstores = sstores_,
                                   ssupply = ss
                                 }
  where
    (nunits, sunits) = M.partition (\ (_, f) -> f == North) pieces_
    nmilitary = M.keysSet $ M.filter (military . fst) nunits
    smilitary = M.keysSet $ M.filter (military . fst) sunits
    nsuppliers = go $ M.keys $ M.filter (not . military . fst) nunits
    ssuppliers = go $ M.keys $ M.filter (not . military . fst) sunits
    ns = resupply nstores_ nsuppliers nmilitary smilitary
    ss = resupply sstores_ ssuppliers smilitary nmilitary

    go :: [Position] -> Supply
    go [] = Zero
    go (p:[]) = One p
    go (p:q:_) = Two p q

-- | An empty board.
bempty :: Board
bempty = mk (M.empty) nstores_ sstores_
  where
    nstores_ = Two (P 8 4) (P 15 2)
    sstores_ = Two (P 3 20) (P 23 20)

-- | Get the positions of one half of the board.
half :: Faction -> S.Set Position
half North = northern
half South = southern

-- | The northern half of the board.
northern :: S.Set Position
northern = S.fromList $ [P u v | u <- [1..width], v <- [1..10]]

-- | The southern half of the board.
southern :: S.Set Position
southern = S.fromList $ [P u v | u <- [1..width], v <- [11..20]]

-- | Move a piece on the board. The result indicates if a store has been
-- destroyed in the process.
mv :: Board -> Position -> Position -> (StoreDiff, Board)
mv b from to = case funit b from of
  Nothing -> (Same, b)
  Just (u, f) -> add (rm b from) to u f
  -- Thanks to laziness, the 'resupply' part should not be computed after
  -- applying 'rm'.

-- | Remove a piece from the board.
rm :: Board -> Position -> Board
rm b p = let pieces' = M.delete p (pieces b)
         in mk pieces' (nstores b) (sstores b)

-- | Add a unit to the board.
add :: Board -> Position -> Unit -> Faction -> (StoreDiff, Board)
add b p u f = (diff, b')
  where
    pieces' = M.insert p (u, f) (pieces b)
    b' = mk pieces' nst sst
    ours = spositions b f
    theirs = spositions b (other f)
    (diff, theirs') = case theirs of
      Zero -> (Same, theirs)
      One q -> if q == p
               then (Second, Zero)
               else (Same, theirs)
      Two q1 q2 -> if q1 == p
                   then (First, One q2)
                   else if q2 == p
                        then (First, One q1)
                        else (Same, theirs)
    nst = if f == North
          then ours
          else theirs'
    sst = if f == South
          then ours
          else theirs'

-- | Add a unit to the board, discarding any information regarding the
-- potential destruction of a store.
add' :: Board -> Position -> Unit -> Faction -> Board
add' b p u f = snd $ add b p u f

-- | Get the distance between two positions (ignore all obstacles), according
-- to the L1 norm.
dist :: Position -> Position -> Int
dist (P x y) (P u v) = max (abs $ x - u) (abs $ y - v)

-- | Get all positions within a given distance of the initial position,
-- according to the L1 norm. The center of the circle (i.e.: the given
-- position) is not included in the result.
circle :: Position -> Int -> S.Set Position
circle (P x y) n = S.fromList $ [P u v | u <- xs, v <- ys]
  where
    xs = [(max 1 (x - n))..(min (x + n) width)]
    ys = [(max 1 (y - n))..(min (y + n) height)]

-- | Get all positions adjacent to the given position.
neighbors :: Position -> S.Set Position
neighbors p = circle p 1

-- | Get all positions aligned with a given position. The rays of the star are
-- blocked by mountains. The center of the star is not included.
star :: Position -> Star
star (P x y) = Star [go $ zip east (repeat y),
                     go $ zip east north,
                     go $ zip (repeat x) north,
                     go $ zip west north,
                     go $ zip west (repeat y),
                     go $ zip west south,
                     go $ zip (repeat x) south,
                     go $ zip east south
                    ]
  where
    east = [(x + 1)..width]
    west = reverse [1..(x - 1)]
    north = reverse [1..(y - 1)]
    south = [(y + 1)..height]

    go :: [(Int, Int)] -> [Position]
    go [] = []
    go ((u, v):xs) = if tile p /= Mountain
                     then p:(go xs)
                     else []
      where
        p = P u v

-- | Get all positions aligned with a given position *within a given range*.
-- The rays of the star are blocked by mountains. The center of the star is
-- not included.
bstar :: Position -> Int -> Star
bstar p n = Star (fmap go xss)
  where
    (Star xss) = star p
    go :: [Position] -> [Position]
    go [] = []
    go (x:xs)
      | d < n = (x:go xs)
      | d == n = [x]
      | otherwise = []
      where
        d = dist x p

-- | Get the unit (if any) at a given position.
unit :: Board -> Position -> Maybe Unit
unit b p = fmap fst (funit b p)

-- | Get the unit (if any) and its faction at a given position.
funit :: Board -> Position -> Maybe (Unit, Faction)
funit b p = M.lookup p (pieces b)

-- | Get the positions of all the units of a given faction.
upositions :: Board -> Faction -> S.Set Position
upositions b f = M.keysSet $ M.filter (\ (_, f') -> f' == f) (pieces b)

-- | Get the positions of the stores of a given faction.
spositions :: Board -> Faction -> Supply
spositions b North = nstores b
spositions b South = sstores b

-- | Test if a position is supplied for the given faction.
supplied :: Board -> Faction -> Position -> Bool
supplied b North p = S.member p (nsupply b)
supplied b South p = S.member p (ssupply b)

-- | Test if a position on the board is empty.
free :: Board -> Position -> Bool
free b p = if tile p == Mountain
  then False
  else case unit b p of
         Nothing -> True
         Just _ -> False

-- | Get the tile at a given position.
tile :: Position -> Tile
tile (P 8 2) = Fortress
tile (P 13 9) = Fortress
tile (P 21 8) = Fortress
tile (P 10 3) = Mountain
tile (P 10 4) = Mountain
tile (P 10 5) = Mountain
tile (P 10 6) = Pass
tile (P 10 7) = Mountain
tile (P 10 8) = Mountain
tile (P 10 9) = Mountain
tile (P 11 3) = Mountain
tile (P 12 3) = Mountain
tile (P 13 3) = Mountain
tile (P 3 13) = Fortress
tile (P 15 12) = Fortress
tile (P 23 15) = Fortress
tile (P 11 14) = Mountain
tile (P 12 14) = Mountain
tile (P 13 14) = Mountain
tile (P 14 14) = Mountain
tile (P 15 14) = Mountain
tile (P 16 14) = Mountain
tile (P 16 15) = Pass
tile (P 16 16) = Mountain
tile (P 16 17) = Mountain
tile (P 16 18) = Mountain
tile (P _ _) = Plain
