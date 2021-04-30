{- |
   Module      : Kriegspiel.Game.Phase
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Phases of the game: placement, movement, etc.
-}

module Kriegspiel.Game.Phase (
  Phase(..),
  Placing'(..),
  Retreating'(..),
  Moving'(..),
  Attacking'(..)
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Kriegspiel.Game.Board

-- | A phase of the game.
data Phase = Placing Placing'
           | Retreating Retreating'
           | Moving Moving'
           | Attacking Attacking'
           | Victory Faction

-- | Informations related to a "Placing" phase.
data Placing' = Placing' { pplayer :: Faction,
                           ptodo :: M.Map Unit Int
                         }

-- | Informations related to a "Retreating" phase.
data Retreating' = Retreating' { rplayer :: Faction,
                                 rshaken :: Position
                               }

-- | Informations related to a "Moving" phase.
data Moving' = Moving' { nmoves :: Int,
                         mplayer :: Faction,
                         mshaken :: Maybe Position,
                         moved :: S.Set Position,
                         attack :: Bool
                       }

-- | Informations related to an "Attacking" phase.
data Attacking' = Attacking' { aplayer :: Faction,
                               ashaken :: Maybe Position
                             }
