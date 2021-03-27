{- |
   Module      : Kriegspiel.Game.Faction
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Factions of the game: North and South.
-}

module Kriegspiel.Game.Faction (
  Faction(..),
  other
  ) where

-- | One of the two factions.
data Faction = North | South

-- | Get the other faction.
other :: Faction -> Faction
other North = South
other South = North
