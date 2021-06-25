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
  northPicture,
  southPicture,
  composed,
  plain,
  mkBitmapLib,
  grid,
  gridTotalWidth,
  gridTotalHeight,
  gridLeftBound,
  gridUpperBound
  ) where

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
                      mountedArtillery :: Composable Colored }

-- | Get the northern version of the picture.
northPicture :: Colored -> Picture
northPicture (Col p _) = p

-- | Get the southern version of the picture.
southPicture :: Colored -> Picture
southPicture (Col _ p) = p

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
  p <- loadBMP (fp ++ "-marker.bmp")
  return (Composable c p)

-- | Load two versions of (one for each faction) a set of pictures that can be
-- "composed" with other pictures.
mkComposableColored :: FilePath -> IO (Composable Colored)
mkComposableColored fp = do
  c <- mkColored (fp ++ "-marker")
  p <- mkColored fp
  return (Composable c p)

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
               mountedArtillery = mart })

-- | Height of the game window (in pixels).
windowHeight :: Int
windowHeight = 600

-- | Width of the game window (in pixels).
windowWidth :: Int
windowWidth = 800

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