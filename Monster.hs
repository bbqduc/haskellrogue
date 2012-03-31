module Monster where

import Vec2

data Monster = Monster { pos :: Vec2 }

moveMonsters :: [Monster] -> Int -> Int -> [Monster]
moveMonsters [] _ _ = []
moveMonsters (m:ms) pX pY = newM:(moveMonsters ms pY pX)
                              where newPos = approach (pos m) (Vec2 pY pX)
			            newM = Monster { pos = newPos }
