module Monster where

import Vec2 as Vec2

data Monster = Monster { pos :: Vec2.Vec2 }

moveMonsters :: [Monster] -> Int -> Int -> [Monster]
moveMonsters [] _ _ = []
moveMonsters (m:ms) pX pY = newM:(moveMonsters ms pY pX)
                              where newPos = Vec2.approach (pos m) (Vec2.Vec2 pY pX)
			            newM = Monster { pos = newPos }
