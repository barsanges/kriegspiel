{- |
   Module      : Main
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Run a game.
-}

module Main where
import Kriegspiel.GUI.Engine

-- | Application entry point.
main :: IO ()
main = runGame "assets/"
