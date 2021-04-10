{- |
   Module      : Kriegspiel.Game.Unit
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

A unit on the board.
-}

module Kriegspiel.Game.Unit (
  Unit(..),
  speed,
  range,
  maxrange,
  ostrength,
  dstrength
  ) where

import Kriegspiel.Game.Tile

-- | A unit on the board.
data Unit = Supply
          | MountedSupply
          | Infantry
          | Cavalry
          | Artillery
          | MountedArtillery
  deriving (Eq, Show)

-- | Get the speed of a unit.
speed :: Unit -> Int
speed Supply = 1
speed MountedSupply = 2
speed Infantry = 1
speed Cavalry = 2
speed Artillery = 1
speed MountedArtillery = 2

-- | Get the range of a unit.
range :: Unit -> Int
range Supply = 2
range MountedSupply = 2
range Infantry = 2
range Cavalry = 2
range Artillery = 3
range MountedArtillery = 3

-- | The maximal range used in combat.
maxrange :: Int
maxrange = 4 -- The maximal range of a charge.

-- | Get the offensive strength of a unit (without taking into account the
-- possibility for the cavalry to charge).
ostrength :: Unit -> Int
ostrength Supply = 0
ostrength MountedSupply = 0
ostrength Infantry = 4
ostrength Cavalry = 4
ostrength Artillery = 5
ostrength MountedArtillery = 5

-- | Get the defensive strength of a unit.
dstrength :: Unit -> Tile -> Int
dstrength Supply _ = 1
dstrength MountedSupply _ = 1
dstrength Infantry Fortress = 10
dstrength Infantry Pass = 8
dstrength Infantry _ = 6
dstrength Cavalry _ = 5
dstrength Artillery Fortress = 12
dstrength Artillery Pass = 10
dstrength Artillery _ = 8
dstrength MountedArtillery t = dstrength Artillery t
