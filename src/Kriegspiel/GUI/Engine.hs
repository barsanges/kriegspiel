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
import System.Directory ( doesFileExist )
import Data.Aeson ( ToJSON, FromJSON, encodeFile, decodeFileStrict' )
import qualified Data.Map as M
import Data.Functor ( ($>) )
import Data.Maybe ( fromMaybe )
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.IO.Game
import Kriegspiel.GUI.Utils
import Kriegspiel.Game.Attack
import Kriegspiel.Game.GameState
import Kriegspiel.Game.Movement
import Kriegspiel.Game.Placement
import Kriegspiel.AI

data Human = NorthPlacement Placing (Maybe Faction) (Maybe Unit) (Maybe Position)
           | SouthPlacement Placing Placing (Maybe Faction) (Maybe Unit) (Maybe Position)
           | PlayerTurn GameState (Maybe Faction) (Maybe Position)
  deriving Generic

data EndTurn = NP Placing
             | SP GameState
             | T GameState (Maybe Faction)

instance ToJSON Human
instance FromJSON Human

data Computer = C Float GameState Turn (Maybe Faction)
  deriving Generic

instance ToJSON Computer
instance FromJSON Computer

data GUI = Menu
         | PvP Human
         | SinglePlayer AI (Either Computer Human)
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
draw blib (PvP h) = drawHuman blib h
draw blib (SinglePlayer _ (Left (C _ gs t ms))) = drawComputer blib gs ms t
draw blib (SinglePlayer _ (Right h)) = drawHuman blib h

drawComputer :: BitmapLib -> GameState -> Maybe Faction -> Turn -> Picture
drawComputer blib (GS p b) mshow t =
  catPictures [Just (displayBoard blib title b mshow (shaken p)),
               displayMovementsLeft blib p,
               fmap (highlightTwice f) p1,
               fmap (highlight f) p2
              ]
  where
    title = phaseTitle' blib p
    f = player p
    (p1, p2) = case t of
      Mov x1 x2 _ -> (Just x1, Just x2)
      Att x1 _ -> (Just x1, Nothing)
      Pas _ -> (Nothing, Nothing)

drawHuman :: BitmapLib -> Human -> Picture
drawHuman blib (NorthPlacement (Placing _ mu b) mshow munit mpos) =
  catPictures [Just (displayBoard blib title b mshow Nothing),
               fmap (highlight North) mpos,
               Just (unitsToPlace blib mu North),
               fmap (highlightPlacement North) munit,
               endPlacementButton blib mu
              ]
  where
    title = phaseTitle North (placementTitle blib)
drawHuman blib (SouthPlacement _ (Placing _ mu b) mshow munit mpos) =
  catPictures [Just (displayBoard blib title b mshow Nothing),
               fmap (highlight South) mpos,
               Just (unitsToPlace blib mu South),
               fmap (highlightPlacement South) munit,
               endPlacementButton blib mu
              ]
  where
    title = phaseTitle South (placementTitle blib)
drawHuman blib (PlayerTurn (GS p b) mshow mpos) = case p of
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
save fp g = (encodeFile fp g) $> g

-- | Handle input events.
handle :: FilePath -> Event -> GUI -> IO GUI
handle fp (EventKey (MouseButton LeftButton) Down _ point) Menu
  | pointInBox point (-100, 60) (100, 20) = do
      test <- doesFileExist fp
      if test
        then fmap (fromMaybe Menu) (decodeFileStrict' fp)
        else pure Menu
  | pointInBox point (-100, -20) (100, -60) = save fp $ SinglePlayer aiSouth (Right $ NorthPlacement (initial North) Nothing Nothing Nothing)
  | pointInBox point (-100, -100) (100, -140) = save fp $ SinglePlayer aiNorth (Right $ SouthPlacement (initialAI aiNorth) (initial South) Nothing Nothing Nothing)
  | pointInBox point (-100, -180) (100, -220) = save fp $ PvP $ NorthPlacement (initial North) Nothing Nothing Nothing
  | otherwise = pure Menu
  where
    aiNorth = mkAI North
    aiSouth = mkAI South
handle fp (EventKey (MouseButton LeftButton) Down _ point) (PvP h) =
  case handleHuman point h of
    Left (NP p) -> save fp (PvP $ SouthPlacement p (initial South) Nothing Nothing Nothing)
    Left (SP gs) -> save fp (PvP $ PlayerTurn gs Nothing Nothing)
    Left (T gs ms) -> save fp (PvP $ PlayerTurn gs ms Nothing)
    Right h' -> save fp (PvP h')
handle fp (EventKey (MouseButton LeftButton) Down _ point) (SinglePlayer ai ch) =
  save fp (SinglePlayer ai ch')
  where
    ch' = case ch of
      Left (C x gs t ms) -> Left (C x gs t (toggleSupply point ms))
      Right h -> case handleHuman point h of
        Left (NP p) -> case merge p (initialAI ai) of
          Nothing -> error "unable to finish the placement phase" -- Should not happen in practice.
          Just gs -> endPl gs
        Left (SP gs) -> endPl gs
        Left (T gs ms) -> Left $ C 0 gs (playAI ai gs) ms
        Right h' -> Right h'
    endPl :: GameState -> Either Computer Human
    endPl (GS p b) = if faction ai == player p
                     then Left $ C 0 (GS p b) (playAI ai (GS p b)) Nothing
                     else Right $ PlayerTurn (GS p b) Nothing Nothing
handle _ _ g = pure g

handleHuman :: (Float, Float) -> Human -> Either EndTurn Human
handleHuman point (NorthPlacement p ms munit mpos) =
  if (done p) && (clickEnd point)
  then Left $ NP p
  else Right $ handlePlacement point p ms munit mpos NorthPlacement
handleHuman point (SouthPlacement p p' ms munit mpos) =
  if clickEnd point
  then case merge p p' of
    Nothing -> Right $ handlePlacement point p' ms munit mpos (SouthPlacement p)
    Just gs -> Left $ SP gs
  else Right $ handlePlacement point p' ms munit mpos (SouthPlacement p)
handleHuman point (PlayerTurn (GS p b) mshow mpos) =
  case p of
    Retreating _ -> handleMovement point (GS p b) mshow mpos
    Moving _ -> handleMovement point (GS p b) mshow mpos
    Attacking _ -> handleAttack point (GS p b) mshow mpos
    Victory _ -> Right $ PlayerTurn (GS p b) Nothing Nothing

handlePlacement :: (Float, Float)
                -> Placing
                -> Maybe Faction
                -> Maybe Unit
                -> Maybe Position
                -> (Placing -> Maybe Faction -> Maybe Unit -> Maybe Position -> Human)
                -> Human
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
               -> Either EndTurn Human
handleMovement point gs ms mpos =
  if player' gsn /= player' gs
  then Left (T gsn msn)
  else Right (PlayerTurn gsn msn mposn)
  where
    (PlayerTurn gsn msn mposn) = case movements gs of
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
    ms' = toggleSupply point ms
    mpos' = clickPosition point

handleAttack :: (Float, Float)
             -> GameState
             -> Maybe Faction
             -> Maybe Position
             -> Either EndTurn Human
handleAttack point gs ms _ = case attacks gs of
  Nil gs' -> if clickEnd point
             then Left (T gs' ms')
             else Right (PlayerTurn gs ms' Nothing)
  Attacks gs' pgs -> if clickEnd point
                     then Left (T gs' ms')
                     else case (mpos >>= (\ k -> M.lookup k pgs)) of
                            Nothing -> Right (PlayerTurn gs ms' Nothing)
                            Just (_, gs'') -> Left (T gs'' ms')
  where
    ms' = toggleSupply point ms
    mpos = clickPosition point

-- | Update the GUI as time goes by.
update :: Float -> GUI -> GUI
update dx (SinglePlayer ai (Left (C x gs t ms)))
  | x' < aiMoveLen = SinglePlayer ai (Left (C x' gs t ms))
  | player p' == faction ai = SinglePlayer ai (Left (C 0 (GS p' b') t' ms))
  | otherwise = SinglePlayer ai (Right (PlayerTurn (GS p' b') ms Nothing))
  where
    x' = x + dx
    t' = playAI ai gs
    (GS p' b') = getState t
update _ g = g

-- | Maximum time span (in seconds) for displaying one AI move
aiMoveLen :: Float
aiMoveLen = 2

-- | Change the first value only if it is 'Nothing'.
changeNothing :: Maybe a -> Maybe a -> Maybe a
changeNothing mx my = case my of
  Nothing -> mx
  Just _ -> my
