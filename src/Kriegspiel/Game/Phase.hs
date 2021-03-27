{- |
   Module      : Kriegspiel.Game.Phase
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Phases of the game: placement, movement, etc.
-}

module Kriegspiel.Game.Phase (
  Phase(..)
  ) where

-- | A phase of the game.
data Phase = Placing | Moving | Attacking | Victory
