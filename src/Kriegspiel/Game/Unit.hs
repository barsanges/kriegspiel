{-# LANGUAGE DeriveGeneric #-}
{- |
   Module      : Kriegspiel.Game.Unit
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

A unit on the board.
-}

module Kriegspiel.Game.Unit (
  Unit(..),
  military,
  speed,
  range,
  maxrange,
  ostrength,
  cstrength,
  dstrength
  ) where

import GHC.Generics
import Data.Aeson ( ToJSON, FromJSON )
import Kriegspiel.Game.Tile

-- | A unit on the board.
data Unit = Supplier
          | MountedSupplier
          | Infantry
          | Cavalry
          | Artillery
          | MountedArtillery
  deriving (Eq, Generic, Ord, Show)

-- | Serialization.
instance ToJSON Unit
instance FromJSON Unit

-- | Test if a unit is a military unit or a supply unit.
military :: Unit -> Bool
military Supplier = False
military MountedSupplier = False
military Infantry = True
military Cavalry = True
military Artillery = True
military MountedArtillery = True

-- | Get the speed of a unit.
speed :: Unit -> Int
speed Supplier = 1
speed MountedSupplier = 2
speed Infantry = 1
speed Cavalry = 2
speed Artillery = 1
speed MountedArtillery = 2

-- | Get the range of a unit.
range :: Unit -> Int
range Supplier = 2
range MountedSupplier = 2
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
ostrength Supplier = 0
ostrength MountedSupplier = 0
ostrength Infantry = 4
ostrength Cavalry = 4
ostrength Artillery = 5
ostrength MountedArtillery = 5

-- | Get the offensive strength of the cavalry when it charges.
cstrength :: Int
cstrength = 7

-- | Get the defensive strength of a unit.
dstrength :: Unit -> Tile -> Int
dstrength Supplier _ = 1
dstrength MountedSupplier _ = 1
dstrength Infantry Fortress = 10
dstrength Infantry Pass = 8
dstrength Infantry _ = 6
dstrength Cavalry _ = 5
dstrength Artillery Fortress = 12
dstrength Artillery Pass = 10
dstrength Artillery _ = 8
dstrength MountedArtillery t = dstrength Artillery t
