module Monster where

import Vec2 as Vec2
import System.Random

data Monster = Monster { pos :: Vec2.Vec2 }

approach :: Monster -> Vec2 -> Monster
approach m v = Monster {pos = Vec2.approach (pos m) v}

moveRandom :: RandomGen g => g -> Monster -> (Monster, g)
moveRandom r m = (Monster { pos=npos }, nr)
                  where (npos, nr) = Vec2.moveRandom r (pos m)

moveMonstersR :: RandomGen g => g -> [Monster] -> Int -> Int -> ([Monster], g)
moveMonstersR r [] _ _ = ([], r)
moveMonstersR r (m:ms) pY pX = (newM:a, b)
                              where (newM, newR)= Monster.moveRandom r m
                                    (a, b) = moveMonstersR newR ms pY pX

moveMonsters :: [Monster] -> Int -> Int -> [Monster]
moveMonsters [] _ _ = []
moveMonsters (m:ms) pY pX = newM:(moveMonsters ms pY pX)
                              where newM = Monster.approach m (Vec2 pX pY)
