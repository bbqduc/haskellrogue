module Monster where

import Vec2 as Vec2

data Monster = Monster { pos :: Vec2.Vec2 }

moveMonsters :: [Monster] -> Vec2.Vec2 -> [Monster]
moveMonsters [] _ = []
moveMonsters (m:ms) v = newM:(moveMonsters ms v)
                              where newPos = Vec2.approach (pos m) v
			            newM = Monster { pos = newPos }
