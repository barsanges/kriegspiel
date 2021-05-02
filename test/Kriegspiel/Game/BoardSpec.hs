{- |
   Module      : Kriegspiel.Game.BoardSpec
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Test the module Kriegspiel.Game.Board.
-}

module Kriegspiel.Game.BoardSpec ( spec ) where

import Test.Hspec

import Data.Maybe ( fromJust )
import qualified Data.Set as S

import Kriegspiel.Game.Board

p_ :: Int -> Int -> Position
p_ x y = fromJust $ mkPosition x y

spec :: Spec
spec = do
  describe "circle" $ do
    it "returns positions from the board (1)" $
      circle (p_ 1 1) 1 `shouldBe` S.fromList [p_ 2 1,
                                               p_ 1 2,
                                               p_ 2 2]

    it "returns positions from the board (2)" $
      circle (p_ 21 8) 1 `shouldBe` S.fromList [p_ 20 7,
                                                p_ 21 7,
                                                p_ 22 7,
                                                p_ 20 8,
                                                p_ 22 8,
                                                p_ 20 9,
                                                p_ 21 9,
                                                p_ 22 9]

    it "returns positions from the board (3)" $
      circle (p_ 11 4) 1 `shouldBe` S.fromList [p_ 10 3,
                                                p_ 11 3,
                                                p_ 12 3,
                                                p_ 10 4,
                                                p_ 12 4,
                                                p_ 10 5,
                                                p_ 11 5,
                                                p_ 12 5]

    it "returns positions from the board (4)" $
      circle (p_ 8 4) 2 `shouldBe` S.fromList [p_ 6 2,
                                               p_ 7 2,
                                               p_ 8 2,
                                               p_ 9 2,
                                               p_ 10 2,
                                               p_ 6 3,
                                               p_ 7 3,
                                               p_ 8 3,
                                               p_ 9 3,
                                               p_ 10 3,
                                               p_ 6 4,
                                               p_ 7 4,
                                               p_ 9 4,
                                               p_ 10 4,
                                               p_ 6 5,
                                               p_ 7 5,
                                               p_ 8 5,
                                               p_ 9 5,
                                               p_ 10 5,
                                               p_ 6 6,
                                               p_ 7 6,
                                               p_ 8 6,
                                               p_ 9 6,
                                               p_ 10 6]

    it "returns positions from the board (5)" $
      circle (p_ 15 10) 3 `shouldBe` S.fromList [p_ 12 7,
                                                 p_ 13 7,
                                                 p_ 14 7,
                                                 p_ 15 7,
                                                 p_ 16 7,
                                                 p_ 17 7,
                                                 p_ 18 7,
                                                 p_ 12 8,
                                                 p_ 13 8,
                                                 p_ 14 8,
                                                 p_ 15 8,
                                                 p_ 16 8,
                                                 p_ 17 8,
                                                 p_ 18 8,
                                                 p_ 12 9,
                                                 p_ 13 9,
                                                 p_ 14 9,
                                                 p_ 15 9,
                                                 p_ 16 9,
                                                 p_ 17 9,
                                                 p_ 18 9,
                                                 p_ 12 10,
                                                 p_ 13 10,
                                                 p_ 14 10,
                                                 p_ 16 10,
                                                 p_ 17 10,
                                                 p_ 18 10,
                                                 p_ 12 11,
                                                 p_ 13 11,
                                                 p_ 14 11,
                                                 p_ 15 11,
                                                 p_ 16 11,
                                                 p_ 17 11,
                                                 p_ 18 11,
                                                 p_ 12 12,
                                                 p_ 13 12,
                                                 p_ 14 12,
                                                 p_ 15 12,
                                                 p_ 16 12,
                                                 p_ 17 12,
                                                 p_ 18 12,
                                                 p_ 12 13,
                                                 p_ 13 13,
                                                 p_ 14 13,
                                                 p_ 15 13,
                                                 p_ 16 13,
                                                 p_ 17 13,
                                                 p_ 18 13]

  describe "star" $ do
    it "gets all positions aligned with a given position (1)" $
      star (p_ 10 6) `shouldBe` Star [[p_ (10 + i) 6 | i <- [1..15]],
                                      [p_ 11 5, p_ 12 4],
                                      [],
                                      [p_ (10 - i) (6 - i) | i <- [1..5]],
                                      [p_ (10 - i) 6 | i <- [1..9]],
                                      [p_ (10 - i) (6 + i) | i <- [1..9]],
                                      [],
                                      [p_ (10 + i) (6 + i) | i <- [1..14]]]

    it "gets all positions aligned with a given position (2)" $
      star (p_ 13 17) `shouldBe` Star [[p_ 14 17, p_ 15 17],
                                       [p_ 14 16, p_ 15 15],
                                       [p_ 13 16, p_ 13 15],
                                       [p_ (13 - i) (17 - i) | i <- [1..12]],
                                       [p_ (13 - i) 17 | i <- [1..12]],
                                       [p_ 12 18, p_ 11 19, p_ 10 20],
                                       [p_ 13 18, p_ 13 19, p_ 13 20],
                                       [p_ 14 18, p_ 15 19, p_ 16 20]]

    it "gets all positions aligned with a given position (3)" $
      star (p_ 3 20) `shouldBe` Star [[p_ (3 + i) 20 | i <- [1..22]],
                                      [p_ (3 + i) (20 - i) | i <- [1..19]],
                                      [p_ 3 (20 - i) | i <- [1..19]],
                                      [p_ 2 19, p_ 1 18],
                                      [p_ 2 20, p_ 1 20],
                                      [],
                                      [],
                                      []]

  describe "bstar" $ do
    it "gets all positions aligned with a given position within a given radius (1)" $
      bstar (p_ 10 6) 1 `shouldBe` Star [[p_ 11 6],
                                         [p_ 11 5],
                                         [],
                                         [p_ 9 5],
                                         [p_ 9 6],
                                         [p_ 9 7],
                                         [],
                                         [p_ 11 7]]

    it "gets all positions aligned with a given position within a given radius (2)" $
      bstar (p_ 10 6) 2 `shouldBe` Star [[p_ 11 6, p_ 12 6],
                                         [p_ 11 5, p_ 12 4],
                                         [],
                                         [p_ 9 5, p_ 8 4],
                                         [p_ 9 6, p_ 8 6],
                                         [p_ 9 7, p_ 8 8],
                                         [],
                                         [p_ 11 7, p_ 12 8]]

    it "gets all positions aligned with a given position within a given radius (3)" $
      bstar (p_ 13 17) 3 `shouldBe` Star [[p_ 14 17, p_ 15 17],
                                          [p_ 14 16, p_ 15 15],
                                          [p_ 13 16, p_ 13 15],
                                          [p_ 12 16, p_ 11 15, p_ 10 14],
                                          [p_ 12 17, p_ 11 17, p_ 10 17],
                                          [p_ 12 18, p_ 11 19, p_ 10 20],
                                          [p_ 13 18, p_ 13 19, p_ 13 20],
                                          [p_ 14 18, p_ 15 19, p_ 16 20]]

    it "gets all positions aligned with a given position within a given radius (4)" $
      bstar (p_ 13 17) 4 `shouldBe` Star [[p_ 14 17, p_ 15 17],
                                          [p_ 14 16, p_ 15 15],
                                          [p_ 13 16, p_ 13 15],
                                          [p_ 12 16, p_ 11 15, p_ 10 14, p_ 9 13],
                                          [p_ 12 17, p_ 11 17, p_ 10 17, p_ 9 17],
                                          [p_ 12 18, p_ 11 19, p_ 10 20],
                                          [p_ 13 18, p_ 13 19, p_ 13 20],
                                          [p_ 14 18, p_ 15 19, p_ 16 20]]

  describe "dist" $ do
    it "computes the L1 distance between two positions (1)" $
      dist (p_ 10 10) (p_ 10 10) `shouldBe` 0

    it "computes the L1 distance between two positions (2)" $
      dist (p_ 9 6) (p_ 10 6) `shouldBe` 1

    it "computes the L1 distance between two positions (3)" $
      dist (p_ 9 6) (p_ 9 7) `shouldBe` 1

    it "computes the L1 distance between two positions (4)" $
      dist (p_ 12 2) (p_ 12 4) `shouldBe` 2

    it "computes the L1 distance between two positions (5)" $
      dist (p_ 1 1) (p_ 25 20) `shouldBe` 24