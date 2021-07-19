{-# LANGUAGE DeriveGeneric #-}
{- |
   Module      : Kriegspiel.GUI.Engine
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Run a game with a GUI.
-}

module Kriegspiel.GUI.Engine (
  runGame
  ) where

import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON, FromJSON, encodeFile, decodeFileStrict' )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.IO.Game
import Kriegspiel.GUI.Utils
import Kriegspiel.Game.Attack
import Kriegspiel.Game.GameState
import Kriegspiel.Game.Movement
import Kriegspiel.Game.Placement

data GUI = Menu
         | NorthPlacement Placing (Maybe Faction) (Maybe Unit) (Maybe Position)
         | SouthPlacement Placing Placing (Maybe Faction) (Maybe Unit) (Maybe Position)
         | PlayerTurn GameState (Maybe Faction) (Maybe Position)
  deriving Generic

instance ToJSON GUI
instance FromJSON GUI

-- | Run a game.
runGame :: FilePath -> FilePath -> IO ()
runGame fp sp = do
  blib <- mkBitmapLib fp
  playIO window
    white
    10 -- Simulation steps per second
    Menu
    (pure . (draw blib))
    (handle sp)
    (\ x w -> pure $ update x w)
  where
    window = InWindow "Kriegspiel" (windowWidth, windowHeight) (10, 10)

-- | Draw the current state of the GUI.
draw :: BitmapLib -> GUI -> Picture
draw blib Menu = pictures [translate 0 (0.25 * (fromIntegral windowHeight)) (gameTitle blib),
                           translate 0 40 (load blib),
                           translate 0 (-40) (northVsIA blib),
                           translate 0 (-120) (southVsIA blib),
                           translate 0 (-200) (twoPlayers blib)]
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
draw blib (PlayerTurn (GS p b) mshow mpos) = case p of
  Retreating _ -> drawMovement blib (GS p b) mshow mpos
  Moving _ -> drawMovement blib (GS p b) mshow mpos
  Attacking _ -> drawAttack blib (GS p b) mshow mpos
  Victory f -> let title = phaseTitle f (victoryTitle blib)
               in displayBoard blib title b Nothing Nothing

drawMovement :: BitmapLib
             -> GameState
             -> Maybe Faction
             -> Maybe Position
             -> Picture
drawMovement blib (GS p b) mshow mpos = case movements (GS p b) of
  None _ -> catPictures [Just (displayBoard blib title b mshow shkn),
                         Just (endMovementButton blib)
                        ]
  Mandatory pos pgs -> catPictures [Just (displayBoard blib title b mshow shkn),
                                    displayMovementsLeft blib p,
                                    Just (highlightTwice f pos),
                                    hght (M.keys pgs)
                                   ]
  Optional _ pgs -> catPictures [Just (displayBoard blib title b mshow (shaken p)),
                                 displayMovementsLeft blib p,
                                 fmap (highlightTwice f) mcurrent,
                                 hght others,
                                 Just (endMovementButton blib)
                                ]
    where
      (mcurrent, others) = case mpos of
        Nothing -> (Nothing, M.keys pgs)
        Just pos -> case M.lookup pos pgs of
          Nothing -> (Nothing, M.keys pgs)
          Just x -> (Just pos, M.keys x)
  where
    f = player p
    shkn = shaken p
    title = phaseTitle f (movementTitle blib)
    hght x = Just (pictures (fmap (highlight f) x))

drawAttack :: BitmapLib
           -> GameState
           -> Maybe Faction
           -> Maybe Position
           -> Picture
drawAttack blib (GS p b) mshow _ = pictures ([displayBoard blib title b mshow (shaken p),
                                              endAttackButton blib]
                                              ++ (fmap (highlight f) defeated)
                                              ++ (fmap (highlightTwice f) destroyed))
  where
    f = player p
    title = phaseTitle f (attackTitle blib)
    (defeated, destroyed) = case attacks (GS p b) of
      Nil _ -> ([], [])
      Attacks _ pgs -> let (x, y) = (M.partition go pgs)
                       in (M.keys x, M.keys y)
    go :: (AttackResult, GameState) -> Bool
    go (a, _) = a == Defeated

-- | Save the state of the game to a file.
save :: FilePath -> GUI -> IO GUI
save fp g = (encodeFile fp g) *> (pure g)

-- | Handle input events.
handle :: FilePath -> Event -> GUI -> IO GUI
handle fp (EventKey (MouseButton LeftButton) Down _ point) Menu
  | pointInBox point (-100, 60) (100, 20) = fmap (\ mg -> fromMaybe Menu mg) (decodeFileStrict' fp)
  | pointInBox point (-100, -20) (100, -60) = pure Menu -- FIXME
  | pointInBox point (-100, -100) (100, -140) = pure Menu -- FIXME
  | pointInBox point (-100, -180) (100, -220) = save fp $ NorthPlacement (initial North) Nothing Nothing Nothing
  | otherwise = pure Menu
handle fp (EventKey (MouseButton LeftButton) Down _ point) (NorthPlacement p ms munit mpos) =
  if clickEnd point
  then save fp $ SouthPlacement p (initial South) Nothing Nothing Nothing
  else save fp $ handlePlacement point p ms munit mpos NorthPlacement
handle fp (EventKey (MouseButton LeftButton) Down _ point) (SouthPlacement p p' ms munit mpos) =
  if clickEnd point
  then case merge p p' of
    Nothing -> save fp $ handlePlacement point p' ms munit mpos (SouthPlacement p)
    Just gs -> save fp $ PlayerTurn gs Nothing Nothing
  else save fp $ handlePlacement point p' ms munit mpos (SouthPlacement p)
handle fp (EventKey (MouseButton LeftButton) Down _ point) (PlayerTurn (GS p b) mshow mpos) =
  case p of
    Retreating _ -> save fp $ handleMovement point (GS p b) mshow mpos
    Moving _ -> save fp $ handleMovement point (GS p b) mshow mpos
    Attacking _ -> save fp $ handleAttack point (GS p b) mshow mpos
    Victory _ -> save fp $ PlayerTurn (GS p b) Nothing Nothing
handle _ _ g = pure g

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

    munit' = changeNothing munit (clickUnitToPlace point)
    mpos' = changeNothing mpos (clickPosition point)

handleMovement :: (Float, Float)
               -> GameState
               -> Maybe Faction
               -> Maybe Position
               -> GUI
handleMovement point gs ms mpos = case movements gs of
  None gs' -> PlayerTurn gs' ms' Nothing
  Mandatory _ pgs -> case (mpos' >>= (\ k -> M.lookup k pgs)) of
    Nothing -> PlayerTurn gs ms' Nothing
    Just gs' -> PlayerTurn gs' ms' Nothing
  Optional gs' pgs -> if clickEnd point
    then PlayerTurn gs' ms' Nothing
    else if mpos == mpos'
         then PlayerTurn gs ms' Nothing
         else case mpos of
           Nothing -> case (mpos' >>= (\ k -> M.lookup k pgs)) of
             Nothing -> PlayerTurn gs ms' mpos
             Just _ -> PlayerTurn gs ms' mpos'
           Just pos -> case M.lookup pos pgs of
             Nothing -> undefined
             Just mp -> case (mpos' >>= (\ k -> M.lookup k mp)) of
               Nothing -> PlayerTurn gs ms' mpos
               Just gs'' -> PlayerTurn gs'' ms' Nothing
  where
    ms' = toggleSupply point ms
    mpos' = clickPosition point

handleAttack :: (Float, Float)
             -> GameState
             -> Maybe Faction
             -> Maybe Position
             -> GUI
handleAttack point gs ms _ = case attacks gs of
  Nil gs' -> if clickEnd point
             then PlayerTurn gs' ms' Nothing
             else PlayerTurn gs ms' Nothing
  Attacks gs' pgs -> if clickEnd point
                     then PlayerTurn gs' ms' Nothing
                     else case (mpos >>= (\ k -> M.lookup k pgs)) of
                            Nothing -> PlayerTurn gs ms' Nothing
                            Just (_, gs'') -> PlayerTurn gs'' ms' Nothing
  where
    ms' = toggleSupply point ms
    mpos = clickPosition point

-- | Update the GUI as time goes by.
update :: Float -> GUI -> GUI
update _ g = g

-- | Change the first value only if it is 'Nothing'.
changeNothing :: Maybe a -> Maybe a -> Maybe a
changeNothing mx my = case my of
  Nothing -> mx
  Just _ -> my
