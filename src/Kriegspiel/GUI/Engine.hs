{- |
   Module      : Kriegspiel.GUI.Engine
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Run a game with a GUI.
-}

module Kriegspiel.GUI.Engine (
  runGame
  ) where

import Graphics.Gloss.Interface.Pure.Game
import Kriegspiel.GUI.Utils

data GUI = Menu

-- | Run a game.
runGame :: FilePath -> IO ()
runGame fp = do
  blib <- mkBitmapLib fp
  play window
    white
    10 -- Simulation steps per second
    Menu
    (draw blib)
    handle
    update
  where
    window = InWindow "Kriegspiel" (windowWidth, windowHeight) (10, 10)

-- | Draw the current state of the GUI.
draw :: BitmapLib -> GUI -> Picture
draw blib Menu = pictures [translate 0 (0.25 * (fromIntegral windowHeight)) (title blib),
                           twoPlayers blib]

-- | Handle input events.
handle :: Event -> GUI -> GUI
handle _ g = g

-- | Update the GUI as time goes by.
update :: Float -> GUI -> GUI
update _ g = g
