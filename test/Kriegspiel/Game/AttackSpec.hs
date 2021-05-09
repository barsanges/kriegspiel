{- |
   Module      : Kriegspiel.Game.AttackSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : Non-Profit Open Software License 3.0

Test the module Kriegspiel.Game.Attack.
-}

module Kriegspiel.Game.AttackSpec ( spec ) where

import Test.Hspec

import Data.Maybe ( fromJust )
import qualified Data.Map as M
import qualified Data.Set as S

import Kriegspiel.Game.GameState
import Kriegspiel.Game.Attack

p_ :: Int -> Int -> Position
p_ x y = fromJust $ mkPosition x y

attacksTest01 :: Attacks
attacksTest01 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = North,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 2, Infantry, North),
                            (p_ 23 20, Infantry, South)]

attacksExp01 :: Attacks
attacksExp01 = Nil (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = South,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 2, Infantry, North),
                            (p_ 23 20, Infantry, South)]

attacksTest02 :: Attacks
attacksTest02 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = North,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 2, Infantry, North)]

attacksExp02 :: Attacks
attacksExp02 = Nil (GS phase b)
  where
    phase = Victory North
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 2, Infantry, North)]

attacksTest03 :: Attacks
attacksTest03 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = North,
                                     ashaken = Just (p_ 8 19)
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 19, Infantry, North),
                            (p_ 8 20, Infantry, South)]

attacksExp03 :: Attacks
attacksExp03 = Nil (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = South,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 19, Infantry, North),
                            (p_ 8 20, Infantry, South)]

attacksTest04 :: Attacks
attacksTest04 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = North,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 7 19, Infantry, North),
                            (p_ 7 20, Supplier, South)]

attacksExp04 :: Attacks
attacksExp04 = Nil (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = South,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 7 19, Infantry, North),
                            (p_ 7 20, Supplier, South)]

attacksTest05 :: Attacks
attacksTest05 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = North,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 4, Infantry, North),
                            (p_ 8 3, Infantry, South)]

attacksExp05 :: Attacks
attacksExp05 = Attacks (GS phase1 b1) (M.fromList [(p_ 8 3, (Destroyed, GS phase2 b2))])
  where
    phase1 = Moving $ Moving' { nmoves = 0,
                                mplayer = South,
                                mshaken = Nothing,
                                moved = S.empty,
                                attack = False
                              }
    b1 = let go (p, u, f) b0 = add' b0 p u f
         in foldr go bempty [(p_ 8 4, Infantry, North),
                             (p_ 8 3, Infantry, South)]
    phase2 = Victory North
    b2 = let go (p, u, f) b0 = add' b0 p u f
         in foldr go bempty [(p_ 8 4, Infantry, North)]

attacksTest06 :: Attacks
attacksTest06 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = North,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 4, MountedSupplier, North),
                            (p_ 8 3, Infantry, South)]

attacksExp06 :: Attacks
attacksExp06 = Nil (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = South,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 4, MountedSupplier, North),
                            (p_ 8 3, Infantry, South)]

attacksTest07 :: Attacks
attacksTest07 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = North,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 12, Infantry, North),
                            (p_ 7 11, Cavalry, North),
                            (p_ 8 11, Cavalry, North),
                            (p_ 8 13, Infantry, South),
                            (p_ 8 15, Supplier, South)]

attacksExp07 :: Attacks
attacksExp07 = Attacks (GS phase1 b) (M.fromList [(p_ 8 13, (Defeated, GS phase2 b))])
  where
    phase1 = Moving $ Moving' { nmoves = 0,
                                mplayer = South,
                                mshaken = Nothing,
                                moved = S.empty,
                                attack = False
                              }
    phase2 = Retreating $ Retreating' { rplayer = South,
                                        rshaken = p_ 8 13
                                      }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 12, Infantry, North),
                            (p_ 7 11, Cavalry, North),
                            (p_ 8 11, Cavalry, North),
                            (p_ 8 13, Infantry, South),
                            (p_ 8 15, Supplier, South)]

attacksTest08 :: Attacks
attacksTest08 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = South,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 10 6, Infantry, North),
                            (p_ 11 6, Infantry, South),
                            (p_ 12 6, Infantry, South),
                            (p_ 12 9, Supplier, South)]

attacksExp08 :: Attacks
attacksExp08 = Nil (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 10 6, Infantry, North),
                            (p_ 11 6, Infantry, South),
                            (p_ 12 6, Infantry, South),
                            (p_ 12 9, Supplier, South)]

attacksTest09 :: Attacks
attacksTest09 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = South,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 15 11, Infantry, North),
                            (p_ 15 12, Infantry, South)]

attacksExp09 :: Attacks
attacksExp09 = Nil (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 15 11, Infantry, North),
                            (p_ 15 12, Infantry, South)]

attacksTest10 :: Attacks
attacksTest10 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = South,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 13 9, Artillery, North),
                            (p_ 13 10, MountedArtillery, North),
                            (p_ 15 10, Infantry, North),
                            (p_ 12 11, Cavalry, South),
                            (p_ 11 12, Cavalry, South),
                            (p_ 10 13, Cavalry, South),
                            (p_ 9 14, Cavalry, South)]

attacksExp10 :: Attacks
attacksExp10 = Attacks (GS phase b1) (M.fromList [(p_ 13 10, (Destroyed, GS phase b2))])
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b1 = let go (p, u, f) b0 = add' b0 p u f
         in foldr go bempty [(p_ 13 9, Artillery, North),
                             (p_ 13 10, MountedArtillery, North),
                             (p_ 15 10, Infantry, North),
                             (p_ 12 11, Cavalry, South),
                             (p_ 11 12, Cavalry, South),
                             (p_ 10 13, Cavalry, South),
                             (p_ 9 14, Cavalry, South)]
    b2 = rm b1 (p_ 13 10)

attacksTest11 :: Bool
attacksTest11 = case attacks (GS phase b) of
                  Nil _ -> False
                  Attacks _ m -> fst (m M.! (p_ 8 15)) == Destroyed
  where
    phase = Attacking $ Attacking' { aplayer = South,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 8 15, Artillery, North),
                            (p_ 7 14, Infantry, North),
                            (p_ 8 14, Infantry, North),
                            (p_ 9 14, Infantry, North),
                            (p_ 7 15, Cavalry, South),
                            (p_ 7 16, Cavalry, South),
                            (p_ 8 16, Cavalry, South),
                            (p_ 9 16, Cavalry, South)]

attacksTest12 :: Attacks
attacksTest12 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = South,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 23 10, Artillery, North),
                            (p_ 23 11, Infantry, South),
                            (p_ 23 12, Cavalry, South)]

attacksExp12 :: Attacks
attacksExp12 = Nil (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 23 10, Artillery, North),
                            (p_ 23 11, Infantry, South),
                            (p_ 23 12, Cavalry, South)]

attacksTest13 :: Attacks
attacksTest13 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = South,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 10 6, Infantry, North),
                            (p_ 11 6, Cavalry, South),
                            (p_ 12 6, Cavalry, South),
                            (p_ 12 9, Supplier, South)]

attacksExp13 :: Attacks
attacksExp13 = Nil (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 10 6, Infantry, North),
                            (p_ 11 6, Cavalry, South),
                            (p_ 12 6, Cavalry, South),
                            (p_ 12 9, Supplier, South)]

attacksTest14 :: Attacks
attacksTest14 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = South,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 21 8, Infantry, North),
                            (p_ 22 8, Cavalry, South),
                            (p_ 23 8, Cavalry, South)]

attacksExp14 :: Attacks
attacksExp14 = Nil (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = North,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 21 8, Infantry, North),
                            (p_ 22 8, Cavalry, South),
                            (p_ 23 8, Cavalry, South)]

attacksTest15 :: Attacks
attacksTest15 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = North,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 10 6, Cavalry, North),
                            (p_ 11 6, Infantry, South),
                            (p_ 11 12, Supplier, South)]

attacksExp15 :: Attacks
attacksExp15 = Attacks (GS phase1 b) (M.fromList [(p_ 11 6, (Defeated, GS phase2 b))])
  where
    phase1 = Moving $ Moving' { nmoves = 0,
                                mplayer = South,
                                mshaken = Nothing,
                                moved = S.empty,
                                attack = False
                              }
    phase2 = Retreating $ Retreating' { rplayer = South,
                                        rshaken = p_ 11 6
                                      }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 10 6, Cavalry, North),
                            (p_ 11 6, Infantry, South),
                            (p_ 11 12, Supplier, South)]

attacksTest16 :: Attacks
attacksTest16 = attacks (GS phase b)
  where
    phase = Attacking $ Attacking' { aplayer = North,
                                     ashaken = Nothing
                                   }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 13 9, Cavalry, North),
                            (p_ 13 10, Infantry, South)]

attacksExp16 :: Attacks
attacksExp16 = Nil (GS phase b)
  where
    phase = Moving $ Moving' { nmoves = 0,
                               mplayer = South,
                               mshaken = Nothing,
                               moved = S.empty,
                               attack = False
                             }
    b = let go (p, u, f) b0 = add' b0 p u f
        in foldr go bempty [(p_ 13 9, Cavalry, North),
                            (p_ 13 10, Infantry, South)]

spec :: Spec
spec = do
  describe "attacks" $ do
    it "it may be impossible to attack" $
      attacksTest01 `shouldBe` attacksExp01

    it "the game ends if a faction has no unit anymore" $
      attacksTest02 `shouldBe` attacksExp02

    it "shaken units cannot attack" $
      attacksTest03 `shouldBe` attacksExp03

    it "non supplied units cannot attack" $
      attacksTest04 `shouldBe` attacksExp04

    it "non supplied, non supported units are immediately destroyed" $
      attacksTest05 `shouldBe` attacksExp05

    it "supply units cannot attack" $
      attacksTest06 `shouldBe` attacksExp06

    it "supply units participate in defense" $
      attacksTest07 `shouldBe` attacksExp07

    it "passes provide a defensive bonus" $
      attacksTest08 `shouldBe` attacksExp08

    it "fortresses provide no offensive bonus" $
      attacksTest09 `shouldBe` attacksExp09

    it "the 4 cavalry units may charge together" $
      attacksTest10 `shouldBe` attacksExp10

    it "the 4 cavalry units may charge from differents directions" $
      attacksTest11 `shouldBe` True

    it "cavalry must be next to the target to charge" $
      attacksTest12 `shouldBe` attacksExp12

    it "cavalry cannot charge towards a pass" $
      attacksTest13 `shouldBe` attacksExp13

    it "cavalry cannot charge towards a fortress" $
      attacksTest14 `shouldBe` attacksExp14

    it "cavalry can charge from a pass" $
      attacksTest15 `shouldBe` attacksExp15

    it "cavalry cannot charge from a fortress" $
      attacksTest16 `shouldBe` attacksExp16
