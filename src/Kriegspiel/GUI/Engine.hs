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
import qualified Data.Map as M
import qualified Data.Set as S
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.Pure.Game
import Kriegspiel.GUI.Utils
import Kriegspiel.Game.GameState
import Kriegspiel.Game.Placement

data GUI = Menu
         | NorthPlacement Placing (Maybe Faction) (Maybe Unit) (Maybe Position)

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
draw blib (NorthPlacement (Placing _ mu b) mshow munit mpos) =
  pictures (catMaybes [Just (phaseTitle North (placementTitle blib)),
                       Just (supplyButton blib mshow),
                       Just grid,
                       fmap (\ f -> showSupply f b S.empty) mshow,
                       fmap (highlight North) mpos,
                       Just (setMarkers blib Nothing b),
                       Just (unitsToPlace blib mu North),
                       fmap (highlightPlacement North) munit,
                       endPlacementButton blib mu
                      ])

-- | Handle input events.
handle :: Event -> GUI -> GUI
handle (EventKey (MouseButton LeftButton) Down _ point) Menu =
  if pointInBox point (-100, 20) (100, -20)
  then NorthPlacement (initial North) Nothing Nothing Nothing
  else Menu
handle (EventKey (MouseButton LeftButton) Down _ point) (NorthPlacement (Placing _ mu b) ms munit mpos) =
  case mpos' of
    Nothing -> NorthPlacement p ms' munit' Nothing
    Just pos -> case M.lookup pos ps of
      Nothing -> NorthPlacement p ms' munit' Nothing
      Just (Left p) -> NorthPlacement p ms' Nothing Nothing
      Just (Right mp) -> case munit' of
        Nothing -> NorthPlacement p ms' Nothing mpos'
        Just u -> case M.lookup u mp of
          Nothing -> NorthPlacement p ms' Nothing mpos'
          Just p' -> NorthPlacement p' ms' Nothing Nothing

  where
    p = Placing North mu b

    ps :: M.Map Position (Either Placing (M.Map Unit Placing))
    ps = placements (Placing North mu b)

    ms' = toggleSupply point ms

    up :: Maybe a -> Maybe a -> Maybe a
    up mx my = case my of
      Nothing -> mx
      Just _ -> my

    munit' = up munit (clickUnitToPlace point)
    mpos' = up mpos (clickPosition point)

handle _ g = g

-- | Update the GUI as time goes by.
update :: Float -> GUI -> GUI
update _ g = g
