module Monster where

import Vec2 as Vec2
import System.Random

data Monster = Monster { pos :: Vec2.Vec2 }

approach :: Monster -> Vec2 -> Monster
approach m v = Monster {pos = Vec2.approach (pos m) v}

moveRandom :: RandomGen g => g -> Monster -> (Monster, g)
moveRandom r m = (Monster { pos=npos }, nr)
                  where (npos, nr) = Vec2.moveRandom r (pos m)

moveMonstersR :: RandomGen g => g -> [Monster] -> Vec2.Vec2 -> ([Monster], g)
moveMonstersR r [] _ = ([], r)
moveMonstersR r (m:ms) v = (newM:a, b)
                              where (newM, newR)= Monster.moveRandom r m
                                    (a, b) = moveMonstersR newR ms v

moveMonsters :: [Monster] -> Vec2.Vec2 -> [Monster]
moveMonsters [] _ = []
moveMonsters (m:ms) v = newM:(moveMonsters ms v)
                              where newM = Monster.approach m v
