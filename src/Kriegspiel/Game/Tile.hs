{- |
   Module      : Kriegspiel.Game.Tile
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

A tile of the board.
-}

module Kriegspiel.Game.Tile (
  Tile(..)
  ) where

-- | A tile of the board.
data Tile = Plain | Fortress | Mountain | Pass
  deriving (Eq, Show)
