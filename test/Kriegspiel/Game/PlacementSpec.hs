{- |
   Module      : Kriegspiel.Game.PlacementSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Test the module Kriegspiel.Game.Placement.
-}

module Kriegspiel.Game.PlacementSpec ( spec ) where

import Test.Hspec

import Data.Maybe ( fromJust )
import qualified Data.Map as M
import qualified Data.Set as S

import Kriegspiel.Game.GameState
import Kriegspiel.Game.Placement
import Kriegspiel.Game.Utils

p_ :: Int -> Int -> Position
p_ x y = fromJust $ mkPosition x y

placementsComp01 :: M.Map Position (Either Placing (M.Map Unit Placing))
placementsComp01 = placements (Placing North todo b)
  where
    todo = M.fromList [(MountedSupplier, 1),
                        (Infantry, 7),
                        (Cavalry, 4),
                        (MountedArtillery, 1)]
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 1 1, Supplier, North),
                            (p_ 2 1, Infantry, North),
                            (p_ 3 1, Infantry, North),
                            (p_ 4 1, Artillery, North)]

placementsTest01 :: M.Map Position (S.Set Unit)
placementsTest01 = M.filter (not . S.null) (M.map go placementsComp01)
  where
    go :: Either Placing (M.Map Unit Placing) -> S.Set Unit
    go (Left _) = S.empty
    go (Right m) = M.keysSet m

placementsExp01 :: M.Map Position (S.Set Unit)
placementsExp01 = fromKeys (\ _ -> us) ps
  where
    ps = S.difference northern (S.fromList [p_ 1 1,
                                            p_ 2 1,
                                            p_ 3 1,
                                            p_ 4 1,
                                            p_ 10 3,
                                            p_ 10 4,
                                            p_ 10 5,
                                            p_ 10 7,
                                            p_ 10 8,
                                            p_ 10 9,
                                            p_ 11 3,
                                            p_ 12 3,
                                            p_ 13 3])
    us = S.fromList [MountedSupplier, Infantry, Cavalry, MountedArtillery]

placementsTest02 :: Maybe Placing
placementsTest02 = case placementsComp01 M.! (p_ 8 2) of
                     Left _ -> Nothing
                     Right m -> M.lookup Cavalry m

placementsExp02 :: Maybe Placing
placementsExp02 = Just (Placing North todo b)
  where
    todo = M.fromList [(MountedSupplier, 1),
                        (Infantry, 7),
                        (Cavalry, 3),
                        (MountedArtillery, 1)]
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 1 1, Supplier, North),
                            (p_ 2 1, Infantry, North),
                            (p_ 3 1, Infantry, North),
                            (p_ 4 1, Artillery, North),
                            (p_ 8 2, Cavalry, North)]

placementsTest03 :: Maybe Placing
placementsTest03 = case placementsComp01 M.! (p_ 1 1) of
                     Left pl -> Just pl
                     Right _ -> Nothing

placementsExp03 :: Maybe Placing
placementsExp03 = Just (Placing North todo b)
  where
    todo = M.fromList [(Supplier, 1),
                        (MountedSupplier, 1),
                        (Infantry, 7),
                        (Cavalry, 4),
                        (MountedArtillery, 1)]
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 2 1, Infantry, North),
                            (p_ 3 1, Infantry, North),
                            (p_ 4 1, Artillery, North)]

mergeTest01 :: Maybe GameState
mergeTest01 = merge mergeComp01North mergeComp01South

mergeComp01North :: Placing
mergeComp01North = (Placing North M.empty b)
  where
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 1 1, Infantry, North),
                            (p_ 2 1, Infantry, North),
                            (p_ 3 1, Infantry, North),
                            (p_ 4 1, Infantry, North),
                            (p_ 5 1, Infantry, North),
                            (p_ 6 1, Infantry, North),
                            (p_ 7 1, Infantry, North),
                            (p_ 8 1, Infantry, North),
                            (p_ 9 1, Infantry, North),
                            (p_ 10 1, Cavalry, North),
                            (p_ 11 1, Cavalry, North),
                            (p_ 12 1, Cavalry, North),
                            (p_ 13 1, Cavalry, North),
                            (p_ 14 1, Artillery, North),
                            (p_ 15 1, MountedArtillery, North),
                            (p_ 16 1, Supplier, North),
                            (p_ 17 1, MountedSupplier, North)]

mergeComp01South :: Placing
mergeComp01South = (Placing South M.empty b)
  where
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 1 20, Infantry, South),
                            (p_ 2 20, Infantry, South),
                            (p_ 3 20, Infantry, South),
                            (p_ 4 20, Infantry, South),
                            (p_ 5 20, Infantry, South),
                            (p_ 6 20, Infantry, South),
                            (p_ 7 20, Infantry, South),
                            (p_ 8 20, Infantry, South),
                            (p_ 9 20, Infantry, South),
                            (p_ 10 20, Cavalry, South),
                            (p_ 11 20, Cavalry, South),
                            (p_ 12 20, Cavalry, South),
                            (p_ 13 20, Cavalry, South),
                            (p_ 14 20, Artillery, South),
                            (p_ 15 20, MountedArtillery, South),
                            (p_ 16 20, Supplier, South),
                            (p_ 17 20, MountedSupplier, South)]

mergeExp01 :: Maybe GameState
mergeExp01 = Just (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 1 1, Infantry, North),
                            (p_ 2 1, Infantry, North),
                            (p_ 3 1, Infantry, North),
                            (p_ 4 1, Infantry, North),
                            (p_ 5 1, Infantry, North),
                            (p_ 6 1, Infantry, North),
                            (p_ 7 1, Infantry, North),
                            (p_ 8 1, Infantry, North),
                            (p_ 9 1, Infantry, North),
                            (p_ 10 1, Cavalry, North),
                            (p_ 11 1, Cavalry, North),
                            (p_ 12 1, Cavalry, North),
                            (p_ 13 1, Cavalry, North),
                            (p_ 14 1, Artillery, North),
                            (p_ 15 1, MountedArtillery, North),
                            (p_ 16 1, Supplier, North),
                            (p_ 17 1, MountedSupplier, North),
                            (p_ 1 20, Infantry, South),
                            (p_ 2 20, Infantry, South),
                            (p_ 3 20, Infantry, South),
                            (p_ 4 20, Infantry, South),
                            (p_ 5 20, Infantry, South),
                            (p_ 6 20, Infantry, South),
                            (p_ 7 20, Infantry, South),
                            (p_ 8 20, Infantry, South),
                            (p_ 9 20, Infantry, South),
                            (p_ 10 20, Cavalry, South),
                            (p_ 11 20, Cavalry, South),
                            (p_ 12 20, Cavalry, South),
                            (p_ 13 20, Cavalry, South),
                            (p_ 14 20, Artillery, South),
                            (p_ 15 20, MountedArtillery, South),
                            (p_ 16 20, Supplier, South),
                            (p_ 17 20, MountedSupplier, South)]

spec :: Spec
spec = do
  describe "placement" $ do
    it "gives all admissible placements for a given game state" $
      placementsTest01 `shouldBe` placementsExp01

    it "allows to place units" $
      placementsTest02 `shouldBe` placementsExp02

    it "allows to remove and put back units already placed" $
      placementsTest03 `shouldBe` placementsExp03

  describe "merge" $ do
    it "merges two half boards" $
      mergeTest01 `shouldBe` mergeExp01

    it "commutes" $ -- Could be turned into a property
      (merge mergeComp01North mergeComp01South) `shouldBe` (merge mergeComp01South mergeComp01North)
