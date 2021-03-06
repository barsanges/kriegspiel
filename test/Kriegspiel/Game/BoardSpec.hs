{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Kriegspiel.Game.BoardSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Test the module Kriegspiel.Game.Board.
-}

module Kriegspiel.Game.BoardSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck

import Data.Aeson ( encode, decode )
import Data.Maybe ( fromJust )
import qualified Data.Set as S

import Kriegspiel.Game.Board

instance Arbitrary Position where
  arbitrary = elements (S.toList $ S.union (half North) (half South))

p_ :: Int -> Int -> Position
p_ x y = fromJust $ mkPosition x y

b1 :: Board
b1 = let go (p, u, f) b = add' b p u f
     in foldr go bempty [(p_ 15 10, Supplier, North),
                         (p_ 17 4, Infantry, North),
                         (p_ 18 10, Cavalry, North),
                         (p_ 25 12, Artillery, North),
                         (p_ 21 8, Infantry, South)]

addTest01 :: (StoreDiff, Maybe (Unit, Faction))
addTest01 = (sd, muf)
  where
    pos = (p_ 8 2)
    (sd, b) = add bempty pos Infantry North
    muf = funit b pos

addTest02 :: (StoreDiff, Maybe (Unit, Faction))
addTest02 = (sd, muf)
  where
    pos = (p_ 23 20)
    (sd, b) = add bempty pos Cavalry South
    muf = funit b pos

addTest03 :: (StoreDiff, Maybe (Unit, Faction))
addTest03 = (sd, muf)
  where
    pos = (p_ 8 4)
    (sd, b) = add bempty pos Cavalry South
    muf = funit b pos

mvTest01 :: (StoreDiff, Maybe (Unit, Faction), Maybe (Unit, Faction))
mvTest01 = (sd, muf1, muf2)
  where
    p1 = (p_ 10 6)
    p2 = (p_ 16 15)
    b = add' bempty p1 Infantry North
    (sd, b') = mv b p1 p2
    muf1 = funit b' p1
    muf2 = funit b' p2

mvTest02 :: (StoreDiff, Maybe (Unit, Faction), Maybe (Unit, Faction))
mvTest02 = (sd, muf1, muf2)
  where
    p1 = (p_ 24 18)
    p2 = (p_ 15 2)
    b = add' bempty p1 Artillery South
    (sd, b') = mv b p1 p2
    muf1 = funit b' p1
    muf2 = funit b' p2

mvTest03 :: (StoreDiff, Maybe (Unit, Faction), Maybe (Unit, Faction))
mvTest03 = (sd, muf1, muf2)
  where
    p1 = (p_ 3 20)
    p2 = (p_ 23 20)
    b = add' bempty p1 MountedArtillery North
    (sd, b') = mv b p1 p2
    muf1 = funit b' p1
    muf2 = funit b' p2

spec :: Spec
spec = do
  describe "positions" $ do
    it "can be encoded as a JSON string" $
      encode (p_ 1 1)  `shouldBe` "{\"j\":1,\"i\":1}"

    it "can be decoded from a JSON string" $
      (decode "{\"i\":2,\"j\":3}" :: Maybe Position) `shouldBe` (mkPosition 2 3)

    it "cannot always be decoded from a JSON string (1)" $
      (decode "TODO" :: Maybe Position) `shouldBe` Nothing

    it "cannot always be decoded from a JSON string (2)" $
      (decode "{\"j\":1,\"i\":30}" :: Maybe Position) `shouldBe` Nothing

    it "the process does not lose any information" $ property $
      \ p -> (decode . encode) p == (Just p :: Maybe Position)

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

  describe "unit" $ do
    it "gets the unit at a given position (1)" $
      unit b1 (p_ 15 10) `shouldBe` Just Supplier

    it "gets the unit at a given position (2)" $
      unit b1 (p_ 17 4) `shouldBe` Just Infantry

    it "gets the unit at a given position (3)" $
      unit b1 (p_ 18 10) `shouldBe` Just Cavalry

    it "gets the unit at a given position (4)" $
      unit b1 (p_ 25 12) `shouldBe` Just Artillery

    it "gets the unit at a given position (5)" $
      unit b1 (p_ 21 8) `shouldBe` Just Infantry

    it "gets the unit at a given position (6)" $
      unit b1 (p_ 15 2) `shouldBe` Nothing

  describe "funit" $ do
    it "gets the unit and its faction at a given position (1)" $
      funit b1 (p_ 15 10) `shouldBe` Just (Supplier, North)

    it "gets the unit and its faction at a given position (2)" $
      funit b1 (p_ 17 4) `shouldBe` Just (Infantry, North)

    it "gets the unit and its faction at a given position (3)" $
      funit b1 (p_ 18 10) `shouldBe` Just (Cavalry, North)

    it "gets the unit and its faction at a given position (4)" $
      funit b1 (p_ 25 12) `shouldBe` Just (Artillery, North)

    it "gets the unit and its faction at a given position (5)" $
      funit b1 (p_ 21 8) `shouldBe` Just (Infantry, South)

    it "gets the unit and its faction at a given position (6)" $
      funit b1 (p_ 15 2) `shouldBe` Nothing

  describe "upositions" $ do
    it "gets the positions of all units of a given faction (1)" $
      upositions b1 North `shouldBe` S.fromList [p_ 15 10,
                                                 p_ 17 4,
                                                 p_ 18 10,
                                                 p_ 25 12]

    it "gets the positions of all units of a given faction (2)" $
      upositions b1 South `shouldBe` S.fromList [p_ 21 8]

  describe "spositions" $ do
    it "gets the positions of the stores of a given faction (1)" $
      spositions b1 North `shouldBe` (Two (p_ 15 2) (p_ 8 4))

    it "gets the positions of the stores of a given faction (2)" $
      spositions b1 South `shouldBe` (Two (p_ 3 20) (p_ 23 20))

  describe "supplied" $ do
    it "tests if a position is supplied for a given faction (1)" $
      supplied b1 North (p_ 15 10) `shouldBe` True

    it "tests if a position is supplied for a given faction (2)" $
      supplied b1 North (p_ 17 4) `shouldBe` True

    it "tests if a position is supplied for a given faction (3)" $
      supplied b1 North (p_ 18 10) `shouldBe` True

    it "tests if a position is supplied for a given faction (4)" $
      supplied b1 North (p_ 25 12) `shouldBe` False

    it "tests if a position is supplied for a given faction (5)" $
      supplied b1 North (p_ 19 9) `shouldBe` True

    it "tests if a position is supplied for a given faction (6)" $
      supplied b1 North (p_ 8 2) `shouldBe` True

    it "tests if a position is supplied for a given faction (7)" $
      supplied b1 South (p_ 3 2) `shouldBe` True

    it "tests if a position is supplied for a given faction (8)" $
      supplied b1 South (p_ 21 8) `shouldBe` False

  describe "add" $ do
    it "adds a unit to the board (1)" $
      addTest01 `shouldBe` (Same, Just (Infantry, North))

    it "adds a unit to the board (2)" $
      addTest02 `shouldBe` (Same, Just (Cavalry, South))

    it "adds a unit to the board (3)" $
      addTest03 `shouldBe` (First, Just (Cavalry, South))

  describe "add'" $ do
    it "adds a unit to the board (1)" $
      funit (add' bempty (p_ 8 2) Infantry North) (p_ 8 2) `shouldBe` Just (Infantry, North)

    it "adds a unit to the board (2)" $
      funit (add' bempty (p_ 23 20) Cavalry South) (p_ 23 20) `shouldBe` Just (Cavalry, South)

    it "adds a unit to the board (3)" $
      funit (add' bempty (p_ 8 4) Cavalry South) (p_ 8 4) `shouldBe` Just (Cavalry, South)

  describe "rm" $ do
    it "removes a unit from the board (1)" $
      rm (add' bempty (p_ 19 12) Infantry North) (p_ 19 12) `shouldBe` bempty

    it "removes a unit from the board (2)" $
      rm (add' bempty (p_ 15 2) Infantry North) (p_ 15 2) `shouldBe` bempty

    it "removes a unit from the board (3)" $
      rm bempty (p_ 15 2) `shouldBe` bempty

  describe "mv" $ do
    it "moves a unit on the board (1)" $
      mvTest01 `shouldBe` (Same, Nothing, Just (Infantry, North))

    it "moves a unit on the board (2)" $
      mvTest02 `shouldBe` (First, Nothing, Just (Artillery, South))

    it "moves a unit on the board (3)" $
      mvTest03 `shouldBe` (Second, Nothing, Just (MountedArtillery, North))
