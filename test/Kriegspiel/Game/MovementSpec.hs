{- |
   Module      : Kriegspiel.Game.MovementSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Test the module Kriegspiel.Game.Movement.
-}

module Kriegspiel.Game.MovementSpec ( spec ) where

import Test.Hspec

import Data.Maybe ( fromJust )
import qualified Data.Map as M
import qualified Data.Set as S

import Kriegspiel.Game.GameState
import Kriegspiel.Game.Movement

p_ :: Int -> Int -> Position
p_ x y = fromJust $ mkPosition x y

extract :: Movements -> Maybe (M.Map Position (S.Set Position))
extract (None _) = Nothing
extract (Mandatory p m) = Just $ M.fromList [(p, M.keysSet m)]
extract (Optional _ m) = Just $ M.map (M.keysSet) m

movementSkipped :: Movements -> Bool
movementSkipped (None (GS p _)) = case p of
  Attacking _ -> True
  _ -> False
movementSkipped _ = False

isVictory :: Movements -> Bool
isVictory (None (GS p _)) = case p of
  Victory _ -> True
  _ -> False
isVictory _ = False

movementsTest01 :: Maybe (M.Map Position (S.Set Position))
movementsTest01 = extract (movements (GS phase b))
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = add' bempty (p_ 8 10) Infantry North

movementsTest02 :: Maybe (M.Map Position (S.Set Position))
movementsTest02 = extract (movements (GS phase b))
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = add' bempty (p_ 8 10) MountedArtillery North

movementsTest03 :: Movements
movementsTest03 = movements (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = add' bempty (p_ 1 1) Infantry North

movementsTest04 :: Movements
movementsTest04 = movements (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 10 6, Cavalry, North),
                            (p_ 9 5, Infantry, South),
                            (p_ 11 5, Infantry, South),
                            (p_ 9 6, Infantry, South),
                            (p_ 11 6, Infantry, South),
                            (p_ 9 7, Infantry, South),
                            (p_ 11 7, Infantry, South)]


movementsTest05 :: Maybe (M.Map Position (S.Set Position))
movementsTest05 = extract (movements (GS phase b))
  where
    phase = Moving $ Moving' { nmoves = 1,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.singleton (p_ 8 10),
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 10, Cavalry, North),
                            (p_ 7 10, Infantry, North)]

movementsTest06 :: Maybe (M.Map Position (S.Set Position))
movementsTest06 = extract (movements (GS phase b))
  where
    phase = Retreating $ Retreating' { rplayer = North,
                                       rshaken = (p_ 8 2)
                                     }
    b = add' bempty (p_ 8 2) Artillery North

movementsTest07 :: Maybe (M.Map Position (S.Set Position))
movementsTest07 = extract (movements (GS phase b))
  where
    phase = Moving $ Moving' { nmoves = 2,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.fromList [(p_ 23 10), (p_ 3 20)],
                               attack = True
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 23 10, Supplier, North),
                            (p_ 3 20, Cavalry, North),
                            (p_ 23 19, Infantry, North)]

movementsTest08 :: Movements
movementsTest08 = movements (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 3 20, Cavalry, North),
                            (p_ 23 20, Infantry, North)]

movementsTest09 :: Bool
movementsTest09 = case movements (GS phase b) of
                    None _ -> False
                    Mandatory _ _ -> False
                    Optional (GS p b') _ -> case p of
                      Attacking _ -> b' == b
                      _ -> False
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = add' bempty (p_ 8 10) Infantry North

movementsTest10 :: Bool
movementsTest10 = case movements (GS phase b) of
                    None _ -> False
                    Mandatory _ _ -> False
                    Optional (GS p b') _ -> case p of
                      Moving m -> (mplayer m == South) && (b' == b)
                      _ -> False
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = True
                             }
    b = add' bempty (p_ 8 10) Infantry North

movementsTest11 :: Bool
movementsTest11 = case movements (GS phase b) of
                    None (GS p b') -> case p of
                      Attacking _ -> b' == b
                      _ -> False
                    Mandatory _ _ -> False
                    Optional _ _ -> False
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.singleton (p_ 8 10),
                               attack = False
                             }
    b = add' bempty (p_ 8 10) Infantry North

movementsTest12 :: Bool
movementsTest12 = case movements (GS phase b) of
                    None _ -> False
                    Mandatory _ _ -> False
                    Optional _ m -> all go (M.toList (m M.! (p_ 8 10)))
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = add' bempty (p_ 8 10) Infantry North
    go :: (Position, GameState) -> Bool
    go (x, (GS u _)) = case u of
      Moving u' -> S.singleton x == moved u'
      _ -> False

movementsTest13 :: Bool
movementsTest13 = case movements (GS phase b) of
                    None (GS _ b') -> unit b' (p_ 3 19) == Nothing
                    Mandatory _ _ -> False
                    Optional _ _ -> False
  where
    phase = Retreating $ Retreating' { rplayer = North,
                                       rshaken = p_ 3 19
                                     }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 3 19, Infantry, North)]

movementsTest14 :: Bool
movementsTest14 = case movements (GS phase b) of
                    None _ -> False
                    Mandatory p m -> (p == (p_ 3 19)) && (not $ S.member (p_ 3 20) (M.keysSet m))
                    Optional _ _ -> False
  where
    phase = Retreating $ Retreating' { rplayer = North,
                                       rshaken = p_ 3 19
                                     }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 3 19, Infantry, North),
                            (p_ 3 9, Supplier, North)]

spec :: Spec
spec = do
  describe "movements" $ do
    it "units on foot may move 1 square" $
      movementsTest01 `shouldBe` Just (M.fromList [(p_ 8 10, S.fromList [p_ 7 9,
                                                                         p_ 8 9,
                                                                         p_ 9 9,
                                                                         p_ 7 10,
                                                                         p_ 9 10,
                                                                         p_ 7 11,
                                                                         p_ 8 11,
                                                                         p_ 9 11])])

    it "mounted units may move 2 squares" $
      movementsTest02 `shouldBe` Just (M.fromList [(p_ 8 10, S.fromList [p_ 6 8,
                                                                         p_ 7 8,
                                                                         p_ 8 8,
                                                                         p_ 9 8,
                                                                         p_ 6 9,
                                                                         p_ 7 9,
                                                                         p_ 8 9,
                                                                         p_ 9 9,
                                                                         p_ 6 10,
                                                                         p_ 7 10,
                                                                         p_ 9 10,
                                                                         p_ 10 10,
                                                                         p_ 6 11,
                                                                         p_ 7 11,
                                                                         p_ 8 11,
                                                                         p_ 9 11,
                                                                         p_ 10 11,
                                                                         p_ 6 12,
                                                                         p_ 7 12,
                                                                         p_ 8 12,
                                                                         p_ 9 12,
                                                                         p_ 10 12])])

    it "non supplied units cannot move" $
      movementSkipped movementsTest03 `shouldBe` True

    it "surrounded units cannot move" $
      movementSkipped movementsTest04 `shouldBe` True

    it "units indirectly supplied may move" $
      movementsTest05 `shouldBe` Just (M.fromList [(p_ 7 10, S.fromList [p_ 6 9,
                                                                         p_ 7 9,
                                                                         p_ 8 9,
                                                                         p_ 6 10,
                                                                         p_ 6 11,
                                                                         p_ 7 11,
                                                                         p_ 8 11])])

    it "shaken units have to  move" $
      movementsTest06 `shouldBe` Just (M.fromList [(p_ 8 2, S.fromList [p_ 7 1,
                                                                        p_ 8 1,
                                                                        p_ 9 1,
                                                                        p_ 7 2,
                                                                        p_ 9 2,
                                                                        p_ 7 3,
                                                                        p_ 8 3,
                                                                        p_ 9 3])])

    it "it is forbidden to destroy two stores in one turn" $
      movementsTest07 `shouldBe` Just (M.fromList [(p_ 23 19, S.fromList [p_ 22 18,
                                                                          p_ 23 18,
                                                                          p_ 24 18,
                                                                          p_ 22 19,
                                                                          p_ 24 19,
                                                                          p_ 22 20,
                                                                          p_ 24 20])])

    it "the game ends if a faction has no store anymore" $
      isVictory movementsTest08 `shouldBe` True

    it "it is possible to pass and moves directly to the attack" $
      movementsTest09 `shouldBe` True

    it "it is impossible to attack if a store has been destroyed during the turn" $
      movementsTest10 `shouldBe` True

    it "a unit cannot move twice in the same turn (1)" $
      movementsTest11 `shouldBe` True

    it "a unit cannot move twice in the same turn (2)" $
      movementsTest12 `shouldBe` True

    it "a unit that cannot retreat is destroyed" $
      movementsTest13 `shouldBe` True

    it "a unit cannot retreat on an ennemy store" $
      movementsTest14 `shouldBe` True
