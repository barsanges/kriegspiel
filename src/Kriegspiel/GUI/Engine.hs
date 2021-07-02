{- |
   Module      : Kriegspiel.GUI.Engine
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Run a game with a GUI.
-}

module Kriegspiel.GUI.Engine (
  runGame
  ) where

import Data.Maybe ( catMaybes )
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.Pure.Game
import Kriegspiel.GUI.Utils
import Kriegspiel.Game.GameState
import Kriegspiel.Game.Placement

data GUI = Menu
         | NorthPlacement Placing

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
draw blib Menu = pictures [translate 0 (0.25 * (fromIntegral windowHeight)) (gameTitle blib),
                           twoPlayers blib]
draw blib (NorthPlacement (Placing _ mu b)) = pictures (catMaybes [Just (phaseTitle North (placementTitle blib)),
                                                                   Just grid,
                                                                   Just (setMarkers blib Nothing b)
                                                                  ])

-- | Handle input events.
handle :: Event -> GUI -> GUI
handle (EventKey (MouseButton LeftButton) Down _ point) Menu = if pointInBox point (-100, 20) (100, -20)
  then NorthPlacement (initial North)
  else Menu
handle _ Menu = Menu
handle _ g = g

-- | Update the GUI as time goes by.
update :: Float -> GUI -> GUI
update _ g = g
