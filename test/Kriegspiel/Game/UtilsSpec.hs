{- |
   Module      : Kriegspiel.Game.UtilsSpec
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Test the module Kriegspiel.Game.Utils.
-}

module Kriegspiel.Game.UtilsSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as M
import qualified Data.Set as S

import Kriegspiel.Game.Utils

spec :: Spec
spec = do
  describe "fromKeys" $ do
    it "works with an empty structure" $
      fromKeys id ([] :: [Int]) `shouldBe` M.empty

    it "works with lists" $
      fromKeys id ([1, 2] :: [Int]) `shouldBe` (M.fromList [(1, 1), (2, 2)])

    it "works with sets" $
      fromKeys id (S.fromList [1, 2] :: S.Set Int) `shouldBe` (M.fromList [(1, 1), (2, 2)])

    it "works with duplicated elements" $
      fromKeys id ([1, 2, 1] :: [Int]) `shouldBe` (M.fromList [(1, 1), (2, 2)])

  describe "fromKeysMaybe" $ do
    it "works with an empty structure" $
      fromKeysMaybe (Just) [] `shouldBe` (M.empty :: M.Map Int Int)

    it "works with lists" $
      fromKeysMaybe (\ x -> if x < 3 then Just x else Nothing) ([1, 2, 3] :: [Int]) `shouldBe` (M.fromList [(1, 1), (2, 2)])

    it "works with sets" $
      fromKeysMaybe (\ x -> if x < 3 then Just x else Nothing) (S.fromList [1, 2, 3] :: S.Set Int) `shouldBe` (M.fromList [(1, 1), (2, 2)])

    it "works with duplicated elements" $
      fromKeysMaybe (\ x -> if x < 3 then Just x else Nothing) ([1, 2, 1, 3] :: [Int]) `shouldBe` (M.fromList [(1, 1), (2, 2)])

  describe "tests" $ do
    it "tests one condition" $
      (tests [\ x -> x > 0] (1 :: Int)) `shouldBe` True

    it "tests several conditions (1)" $
      (tests [\ x -> x > 0, \ x -> x < 10] (1 :: Int)) `shouldBe` True

    it "tests several conditions (2)" $
      (tests [\ x -> x > 0, \ x -> x < 0] (1 :: Int)) `shouldBe` False

    it "results do not depend on the order of the conditions" $ property $
      \ x y -> (tests [\ a -> a > y - 10, \ a -> a < y + 10] x) == (tests [\ a -> a < y + 10, \ a -> a > y - 10] (x :: Int))
