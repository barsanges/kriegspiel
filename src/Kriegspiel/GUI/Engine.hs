{- |
   Module      : Kriegspiel.GUI.Engine
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Run a game with a GUI.
-}

module Kriegspiel.GUI.Engine (
  runGame
  ) where

import qualified Data.Map as M
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.Pure.Game
import Kriegspiel.GUI.Utils
import Kriegspiel.Game.GameState
import Kriegspiel.Game.Placement

data GUI = Menu
         | NorthPlacement Placing (Maybe Faction) (Maybe Unit) (Maybe Position)
         | SouthPlacement Placing Placing (Maybe Faction) (Maybe Unit) (Maybe Position)

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
  catPictures [Just (displayBoard blib title b mshow Nothing),
               fmap (highlight North) mpos,
               Just (unitsToPlace blib mu North),
               fmap (highlightPlacement North) munit,
               endPlacementButton blib mu
              ]
  where
    title = phaseTitle North (placementTitle blib)
draw blib (SouthPlacement _ (Placing _ mu b) mshow munit mpos) =
  catPictures [Just (displayBoard blib title b mshow Nothing),
               fmap (highlight South) mpos,
               Just (unitsToPlace blib mu South),
               fmap (highlightPlacement South) munit,
               endPlacementButton blib mu
              ]
  where
    title = phaseTitle South (placementTitle blib)

-- | Handle input events.
handle :: Event -> GUI -> GUI
handle (EventKey (MouseButton LeftButton) Down _ point) Menu =
  if pointInBox point (-100, 20) (100, -20)
  then NorthPlacement (initial North) Nothing Nothing Nothing
  else Menu
handle (EventKey (MouseButton LeftButton) Down _ point) (NorthPlacement p ms munit mpos) =
  if clickEndPlacement point
  then SouthPlacement p (initial South) Nothing Nothing Nothing
  else handlePlacement point p ms munit mpos NorthPlacement
handle (EventKey (MouseButton LeftButton) Down _ point) (SouthPlacement p p' ms munit mpos) =
  handlePlacement point p' ms munit mpos (SouthPlacement p)
handle _ g = g

handlePlacement :: (Float, Float)
                -> Placing
                -> Maybe Faction
                -> Maybe Unit
                -> Maybe Position
                -> (Placing -> Maybe Faction -> Maybe Unit -> Maybe Position -> GUI)
                -> GUI
handlePlacement point p ms munit mpos ctor = case mpos' of
    Nothing -> ctor p ms' munit' Nothing
    Just pos -> case M.lookup pos ps of
      Nothing -> ctor p ms' munit' Nothing
      Just (Left p') -> ctor p' ms' Nothing Nothing
      Just (Right mp) -> case munit' of
        Nothing -> ctor p ms' Nothing mpos'
        Just u -> case M.lookup u mp of
          Nothing -> ctor p ms' Nothing mpos'
          Just p' -> ctor p' ms' Nothing Nothing
  where
    ps :: M.Map Position (Either Placing (M.Map Unit Placing))
    ps = placements p

    ms' = toggleSupply point ms

    up :: Maybe a -> Maybe a -> Maybe a
    up mx my = case my of
      Nothing -> mx
      Just _ -> my

    munit' = up munit (clickUnitToPlace point)
    mpos' = up mpos (clickPosition point)

-- | Update the GUI as time goes by.
update :: Float -> GUI -> GUI
update _ g = g
