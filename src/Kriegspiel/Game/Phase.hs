{-# LANGUAGE DeriveGeneric #-}
{- |
   Module      : Kriegspiel.Game.Phase
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Phases of the game: placement, movement, etc.
-}

module Kriegspiel.Game.Phase (
  Phase(..),
  Retreating'(..),
  Moving'(..),
  Attacking'(..),
  player,
  shaken
  ) where

import GHC.Generics ( Generic )
import qualified Data.Set as S
import Data.Aeson ( ToJSON, FromJSON )
import Kriegspiel.Game.Board

-- | A phase of the game.
data Phase = Retreating Retreating'
           | Moving Moving'
           | Attacking Attacking'
           | Victory Faction
  deriving (Eq, Generic, Show)

-- | Informations related to a "Retreating" phase.
data Retreating' = Retreating' { rplayer :: Faction,
                                 rshaken :: Position
                               }
  deriving (Eq, Generic, Show)

-- | Informations related to a "Moving" phase.
data Moving' = Moving' { nmoves :: Int,
                         mplayer :: Faction,
                         mshaken :: Maybe Position,
                         moved :: S.Set Position,
                         attack :: Bool
                       }
  deriving (Eq, Generic, Show)

-- | Informations related to an "Attacking" phase.
data Attacking' = Attacking' { aplayer :: Faction,
                               ashaken :: Maybe Position
                             }
  deriving (Eq, Generic, Show)

-- | Serialization.
instance ToJSON Phase
instance FromJSON Phase

instance ToJSON Retreating'
instance FromJSON Retreating'

instance ToJSON Moving'
instance FromJSON Moving'

instance ToJSON Attacking'
instance FromJSON Attacking'

-- | Indicates the current player.
player :: Phase -> Faction
player (Retreating r) = rplayer r
player (Moving m) = mplayer m
player (Attacking a) = aplayer a
player (Victory f) = f

-- | Get the coordinate of the currently shaken unit, if any.
shaken :: Phase -> Maybe Position
shaken (Retreating r) = Just (rshaken r)
shaken (Moving m) = mshaken m
shaken (Attacking a) = ashaken a
shaken (Victory _) = Nothing
