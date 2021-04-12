{- |
   Module      : Kriegspiel.Game.GameState
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

State of the game, i.e. board and current phase.
-}

module Kriegspiel.Game.GameState (
  module Kriegspiel.Game.Board,
  module Kriegspiel.Game.Phase,
  GameState(..)
  ) where

import Kriegspiel.Game.Board
import Kriegspiel.Game.Phase

-- | State of the game, i.e. board and current phase.
data GameState = GS Phase Board