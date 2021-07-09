{- |
   Module      : Kriegspiel.GUI.Utils
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Common functions used to create/manipulate the GUI.
-}

module Kriegspiel.GUI.Utils (
  BitmapLib(..),
  windowHeight,
  windowWidth,
  mkBitmapLib,
  catPictures,
  grid,
  displayBoard,
  highlight,
  highlightTwice,
  highlightPlacement,
  phaseTitle,
  supplyButton,
  toggleSupply,
  endPlacementButton,
  clickEndPlacement,
  unitsToPlace,
  clickUnitToPlace,
  clickPosition,
  gridTotalWidth,
  gridTotalHeight,
  gridLeftBound,
  gridUpperBound
  ) where

import Data.Maybe ( catMaybes )
import qualified Data.Map as M
import qualified Data.Set as S
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap ( loadBMP )
import Kriegspiel.Game.Board

-- | Two versions of the same picture, one for each faction.
data Colored = Col Picture Picture

-- | A picture or a set of pictures that can be "composed" with other pictures.
data Composable a = Composable a a

-- | Container for all external (bitmap) pictures used in the game.
data BitmapLib = BL { mountain :: Picture,
                      fortress :: Composable Picture,
                      pass :: Composable Picture,
                      store :: Composable Colored,
                      shaken :: Colored,
                      supplier :: Composable Colored,
                      mountedSupplier :: Composable Colored,
                      infantry :: Composable Colored,
                      cavalry :: Composable Colored,
                      artillery :: Composable Colored,
                      mountedArtillery :: Composable Colored,
                      gameTitle :: Picture,
                      placementTitle :: Colored,
                      endPlacement :: Picture,
                      showNoSupply :: Picture,
                      showNorthSupply :: Picture,
                      showSouthSupply :: Picture,
                      numbers :: [Picture],
                      twoPlayers :: Picture
                    }

-- | Get the northern version of the picture.
northPicture :: Colored -> Picture
northPicture (Col p _) = p

-- | Get the southern version of the picture.
southPicture :: Colored -> Picture
southPicture (Col _ p) = p

-- | Get a faction specific version of a picture.
selectColor :: Faction -> (Colored -> Picture)
selectColor North = northPicture
selectColor South = southPicture

-- | Get the northern version of a composable picture.
northComposable :: Composable Colored -> Composable Picture
northComposable (Composable c p) = Composable (northPicture c) (northPicture p)

-- | Get the southern version of a composable picture.
southComposable :: Composable Colored -> Composable Picture
southComposable (Composable c p) = Composable (southPicture c) (southPicture p)

-- | Get a faction specific version of a composable picture.
selectComposable :: Faction -> (Composable Colored -> Composable Picture)
selectComposable North = northComposable
selectComposable South = southComposable

-- | Get the version of the picture that is meant to be composed with other
-- pictures.
composed :: Composable a -> a
composed (Composable c _) = c

-- | Get the plain version of the picture.
plain :: Composable a -> a
plain (Composable _ p) = p

-- | Load two versions of the same picture, one for each faction.
mkColored :: FilePath -> IO Colored
mkColored fp = do
  n <- loadBMP (fp ++ "-north.bmp")
  s <- loadBMP (fp ++ "-south.bmp")
  return (Col n s)

-- | Load a picture or a set of pictures that can be "composed" with other
-- pictures.
mkComposablePicture :: FilePath -> IO (Composable Picture)
mkComposablePicture fp = do
  c <- loadBMP (fp ++ "-marker.bmp")
  p <- loadBMP (fp ++ ".bmp")
  return (Composable c p)

-- | Load two versions of (one for each faction) a set of pictures that can be
-- "composed" with other pictures.
mkComposableColored :: FilePath -> IO (Composable Colored)
mkComposableColored fp = do
  c <- mkColored (fp ++ "-marker")
  p <- mkColored fp
  return (Composable c p)

-- | Load number pictures.
mkNumbers :: FilePath -> IO [Picture]
mkNumbers fp = do
  zero <- loadBMP (fp ++ "00.bmp")
  one <- loadBMP (fp ++ "01.bmp")
  two <- loadBMP (fp ++ "02.bmp")
  three <- loadBMP (fp ++ "03.bmp")
  four <- loadBMP (fp ++ "04.bmp")
  five <- loadBMP (fp ++ "05.bmp")
  six <- loadBMP (fp ++ "06.bmp")
  seven <- loadBMP (fp ++ "07.bmp")
  eight <- loadBMP (fp ++ "08.bmp")
  nine <- loadBMP (fp ++ "09.bmp")
  return [zero, one, two, three, four, five, six, seven, eight, nine]

-- | Load all external (bitmap) pictures used in the game.
mkBitmapLib :: FilePath -> IO BitmapLib
mkBitmapLib fp = do
  mntn <- loadBMP (fp ++ "mountain.bmp")
  fort <- mkComposablePicture (fp ++ "fortress")
  pss <- mkComposablePicture (fp ++ "pass")
  st <- mkComposableColored (fp ++ "store")
  shkn <- mkColored (fp ++ "shaken-marker")
  sup <- mkComposableColored (fp ++ "supplier")
  msup <- mkComposableColored (fp ++ "mounted-supplier")
  inf <- mkComposableColored (fp ++ "infantry")
  cav <- mkComposableColored (fp ++ "cavalry")
  art <- mkComposableColored (fp ++ "artillery")
  mart <- mkComposableColored (fp ++ "mounted-artillery")
  gt <- loadBMP (fp ++ "game-title.bmp")
  pt <- mkColored (fp ++ "placement-title")
  ep <- loadBMP (fp ++ "end-placement.bmp")
  no <- loadBMP (fp ++ "supply-none.bmp")
  ns <- loadBMP (fp ++ "supply-north.bmp")
  ss <- loadBMP (fp ++ "supply-south.bmp")
  nb <- mkNumbers fp
  twoP <- loadBMP (fp ++ "two-players.bmp")
  return (BL { mountain  = mntn,
               fortress = fort,
               pass = pss,
               store = st,
               shaken = shkn,
               supplier = sup,
               mountedSupplier = msup,
               infantry = inf,
               cavalry = cav,
               artillery = art,
               mountedArtillery = mart,
               gameTitle = gt,
               placementTitle = pt,
               endPlacement = ep,
               showNoSupply = no,
               showNorthSupply = ns,
               showSouthSupply = ss,
               numbers = nb,
               twoPlayers = twoP })

-- | Height of the game window (in pixels).
windowHeight :: Int
windowHeight = 600

-- | Width of the game window (in pixels).
windowWidth :: Int
windowWidth = 800

-- | Concatenate pictures.
catPictures :: [Maybe Picture] -> Picture
catPictures = pictures . catMaybes

-- | An empty grid delimiting the board.
grid :: Picture
grid = pictures (rows ++ cols)
  where
    row = rectangleSolid gridTotalWidth lineWidth
    placeRow n = translate (gridLeftBound + 0.5 * gridTotalWidth - 1) (gridUpperBound - (fromIntegral n) * (lineWidth + cellEdge)) row
    rows = fmap placeRow [0..height]
    col = rectangleSolid lineWidth gridTotalHeight
    placeCol n = translate (gridLeftBound + (fromIntegral n) * (lineWidth + cellEdge)) (gridUpperBound - 0.5 * gridTotalHeight) col
    cols = fmap placeCol [0..width]

-- | Total width of the board (in pixels).
gridTotalWidth :: Float
gridTotalWidth = (fromIntegral width) * cellEdge + (fromIntegral $ width + 1) * lineWidth

-- | Total height of the board (in pixels).
gridTotalHeight :: Float
gridTotalHeight = (fromIntegral height) * cellEdge + (fromIntegral $ height + 1) * lineWidth

-- | The leftmost abscissa of the grid.
gridLeftBound :: Float
gridLeftBound = (-0.5) * (fromIntegral windowWidth) + 10

-- | The highest ordinate of the grid.
gridUpperBound :: Float
gridUpperBound = 0.5 * gridTotalHeight

-- | Length of the edge of a board cell (in pixels).
cellEdge :: Float
cellEdge = 24

-- | Width of the line between two cells (in pixels).
lineWidth :: Float
lineWidth = 1

-- | Place a cell at a given position on the grid.
place :: Position -> Picture -> Picture
place pos p = translate x y p
  where
    (i, j) = toInt pos
    x = gridLeftBound + cellEdge * (-0.5 + fromIntegral i) + lineWidth * (fromIntegral i) - 1
    y = gridUpperBound - cellEdge * (-0.5 + fromIntegral j) - lineWidth * (fromIntegral j)

-- | Draw the markers on the board.
setMarkers :: BitmapLib -> Maybe Position -> Board -> Picture
setMarkers blib mp b = pictures (catMaybes (fmap go allPositions))
  where
    allPositions = S.toList whole

    plain' :: Position -> (Composable Picture -> Picture)
    plain' p = case mp of
      Nothing -> plain
      Just p' -> if p /= p'
                 then plain
                 else composed

    go :: Position -> Maybe Picture
    go pos = fmap (place pos) pic
      where
        mtp = getTile blib pos b
        pic = case funit b pos of
                Nothing -> fmap (plain' pos) mtp
                Just (u, f) -> case mtp of
                  Nothing -> Just $ pictures (((plain' pos) up):shakenp)
                  Just tp -> Just $ pictures ([composed tp, composed up] ++ shakenp)
                  where
                    up = getUnit blib f u
                    shakenp = if (Just pos) == mp
                              then [(selectColor f) (shaken blib)]
                              else []

-- | Get the corresponding picture for a given tile.
getTile :: BitmapLib -> Position -> Board -> Maybe (Composable Picture)
getTile blib pos b = case tile pos of
  Mountain -> Just $ Composable (mountain blib) (mountain blib)
  Fortress -> Just $ fortress blib
  Pass -> Just $ pass blib
  Plain -> case mstore b pos of
    Nothing -> Nothing
    Just f -> Just $ (selectComposable f) (store blib)

-- | Get the corresponding picture for a given unit.
getUnit :: BitmapLib -> Faction -> Unit -> Composable Picture
getUnit blib f u = case u of
  Supplier -> (selectComposable f) (supplier blib)
  MountedSupplier -> (selectComposable f) (mountedSupplier blib)
  Infantry -> (selectComposable f) (infantry blib)
  Cavalry -> (selectComposable f) (cavalry blib)
  Artillery -> (selectComposable f) (artillery blib)
  MountedArtillery -> (selectComposable f) (mountedArtillery blib)

-- | Show all supplied positions for a given faction.
showSupply :: Faction -> Board -> S.Set Position -> Picture
showSupply f b xs = pictures (catMaybes (fmap go allPositions))
  where
    allPositions = S.toList whole
    col = case f of
      North -> makeColor 1 0 0 0.2
      South -> makeColor 0 0 1 0.2
    pic = rectangleSolid cellEdge cellEdge

    go :: Position -> Maybe Picture
    go pos = if (supplied b f pos) && (S.notMember pos xs)
             then Just $ place pos (color col pic)
             else Nothing

-- | Display the board.
displayBoard :: BitmapLib
             -> Picture
             -> Board
             -> Maybe Faction
             -> Maybe Position
             -> Picture
displayBoard blib title b mshow mshaken =
  catPictures [Just title,
               Just (supplyButton blib mshow),
               Just grid,
               fmap (\ f -> showSupply f b S.empty) mshow,
               Just (setMarkers blib mshaken b)
              ]

-- | Common function to highlight a cell (not public).
baseHighlight :: Faction -> Picture
baseHighlight f = color col pic
  where
    col = case f of
      North -> red
      South -> blue
    pic = pictures [rectangleWire (cellEdge + 3.5) (cellEdge + 3),
                    rectangleWire (cellEdge + 1.5) (cellEdge + 2),
                    rectangleWire (cellEdge + 0.5) (cellEdge + 1),
                    rectangleWire (cellEdge - 0.5) (cellEdge - 1)]

-- | Highlight a cell.
highlight :: Faction -> Position -> Picture
highlight f pos = place pos (baseHighlight f)

-- | Highlight a cell in two ways.
highlightTwice :: Faction -> Position -> Picture
highlightTwice f pos = pictures [highlight f pos, pic]
  where
    col = case f of
      North -> makeColor 1 0 0 0.4
      South -> makeColor 0 0 1 0.4
    pic = place pos (color col (rectangleSolid cellEdge cellEdge))

-- | Place the title of the phase.
phaseTitle :: Faction -> Colored -> Picture
phaseTitle f pic = translate 0 (0.5 * gridTotalHeight + 20) ((selectColor f) pic)

-- | Display a button for supply lines.
supplyButton :: BitmapLib -> Maybe Faction -> Picture
supplyButton blib mf = translate x y pic
  where
    x = gridLeftBound + 0.5 * gridTotalWidth
    y = (-0.5) * gridTotalHeight - 25
    pic = case mf of
            Nothing -> showNoSupply blib
            Just North -> showNorthSupply blib
            Just South -> showSouthSupply blib

-- | Toggle the display of supply lines.
toggleSupply :: (Float, Float) -> Maybe Faction -> Maybe Faction
toggleSupply point ms = if pointInBox point (x, y) (x + 450, y - 36)
                        then case ms of
                               Nothing -> Just North
                               Just North -> Just South
                               Just South -> Nothing
                        else ms
  where
    x = gridLeftBound + 0.5 * gridTotalWidth - 0.5 * 450
    y = (-0.5) * gridTotalHeight - 25 + 18

-- | Display a button to end the placement phase.
endPlacementButton :: BitmapLib -> M.Map Unit Int -> Maybe Picture
endPlacementButton blib mu = if mu == M.empty
  then Just (translate x y (endPlacement blib))
  else Nothing
  where
    x = gridLeftBound + gridTotalWidth + 2.75 * cellEdge
    y = (-0.5) * gridTotalHeight + 7

-- | TODO
clickEndPlacement :: (Float, Float) -> Bool
clickEndPlacement point = pointInBox point (x, y) (x + 100, y - 100)
  where
    x = gridLeftBound + gridTotalWidth + 2.75 * cellEdge - 50
    y = (-0.5) * gridTotalHeight + 7 + 50

-- | Show which units should be placed on the board.
unitsToPlace :: BitmapLib -> M.Map Unit Int -> Faction -> Picture
unitsToPlace blib mu f = pictures [translate x1 (0.5 * gridTotalHeight - 4.5 * (cellEdge + 1)) (pic (infantry blib)),
                                   translate x2 (0.5 * gridTotalHeight - 4.5 * (cellEdge + 1)) (nb Infantry),

                                   translate x1 (0.5 * gridTotalHeight - 6.5 * (cellEdge + 1)) (pic (cavalry blib)),
                                   translate x2 (0.5 * gridTotalHeight - 6.5 * (cellEdge + 1)) (nb Cavalry),

                                   translate x1 (0.5 * gridTotalHeight - 8.5 * (cellEdge + 1)) (pic (artillery blib)),
                                   translate x2 (0.5 * gridTotalHeight - 8.5 * (cellEdge + 1)) (nb Artillery),

                                   translate x1 (0.5 * gridTotalHeight - 10.5 * (cellEdge + 1)) (pic (mountedArtillery blib)),
                                   translate x2 (0.5 * gridTotalHeight - 10.5 * (cellEdge + 1)) (nb MountedArtillery),

                                   translate x1 (0.5 * gridTotalHeight - 13.5 * (cellEdge + 1)) (pic (supplier blib)),
                                   translate x2 (0.5 * gridTotalHeight - 13.5 * (cellEdge + 1)) (nb Supplier),

                                   translate x1 (0.5 * gridTotalHeight - 15.5 * (cellEdge + 1)) (pic (mountedSupplier blib)),
                                   translate x2 (0.5 * gridTotalHeight - 15.5 * (cellEdge + 1)) (nb MountedSupplier)
                                  ]
  where
    x1 = gridLeftBound + gridTotalWidth + 2 * cellEdge
    x2 = x1 + 1.5 * cellEdge

    pic :: Composable Colored -> Picture
    pic c = pictures [(selectColor f) (plain c),
                     rectangleWire cellEdge cellEdge]

    nb :: Unit -> Picture
    nb u = (numbers blib) !! (M.findWithDefault 0 u mu)

-- | Highlight a unit type during the placement phase.
highlightPlacement :: Faction -> Unit -> Picture
highlightPlacement f u = translate x y pic
  where
    pic = baseHighlight f
    x = gridLeftBound + gridTotalWidth + 2 * cellEdge
    y = case u of
          Infantry -> 0.5 * gridTotalHeight - 4.5 * (cellEdge + 1)
          Cavalry -> 0.5 * gridTotalHeight - 6.5 * (cellEdge + 1)
          Artillery -> 0.5 * gridTotalHeight - 8.5 * (cellEdge + 1)
          MountedArtillery -> 0.5 * gridTotalHeight - 10.5 * (cellEdge + 1)
          Supplier -> 0.5 * gridTotalHeight - 13.5 * (cellEdge + 1)
          MountedSupplier -> 0.5 * gridTotalHeight - 15.5 * (cellEdge + 1)

-- | Get the unit to place associated to a click on the screen.
clickUnitToPlace :: (Float, Float) -> Maybe Unit
clickUnitToPlace p
  | go (0.5 * gridTotalHeight - 4.5 * (cellEdge + 1)) = Just Infantry
  | go (0.5 * gridTotalHeight - 6.5 * (cellEdge + 1)) = Just Cavalry
  | go (0.5 * gridTotalHeight - 8.5 * (cellEdge + 1)) = Just Artillery
  | go (0.5 * gridTotalHeight - 10.5 * (cellEdge + 1)) = Just MountedArtillery
  | go (0.5 * gridTotalHeight - 13.5 * (cellEdge + 1)) = Just Supplier
  | go (0.5 * gridTotalHeight - 15.5 * (cellEdge + 1)) = Just MountedSupplier
  | otherwise = Nothing
  where
    x1 = gridLeftBound + gridTotalWidth + 1.5 * cellEdge
    x2 = x1 + cellEdge
    go :: Float -> Bool
    go y0 = pointInBox p (x1, y0 - 0.5 * cellEdge) (x2, y0 + 0.5 * cellEdge)

-- | Get the position on the board associated to a click on the screen.
clickPosition :: (Float, Float) -> Maybe Position
clickPosition (x, y) = mkPosition i j
  where
    i = 1 + ((truncate $ x - gridLeftBound) `div` (truncate $ cellEdge + lineWidth))
    j = 1 + ((truncate $ gridUpperBound - y) `div` (truncate $ cellEdge + lineWidth))
