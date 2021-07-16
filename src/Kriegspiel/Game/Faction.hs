{-# LANGUAGE DeriveGeneric #-}
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

import GHC.Generics
import Data.Aeson ( ToJSON, FromJSON )

-- | One of the two factions.
data Faction = North | South
  deriving (Eq, Generic, Show)

-- | Serialization.
instance ToJSON Faction
instance FromJSON Faction

-- | Get the other faction.
other :: Faction -> Faction
other North = South
other South = North
