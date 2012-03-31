module Vec2 where

data Vec2 = Vec2 Int Int
            deriving Show

getX :: Vec2 -> Int
getX (Vec2 x _) = x

getY :: Vec2 -> Int
getY (Vec2 _ y) = y

incX :: Vec2 -> Vec2
incX (Vec2 x y) = Vec2 (x+1) y

incY :: Vec2 -> Vec2
incY (Vec2 x y) = Vec2 x (y+1)

decX :: Vec2 -> Vec2
decX (Vec2 x y) = Vec2 (x-1) y

decY :: Vec2 -> Vec2
decY (Vec2 x y) = Vec2 x (y-1)

approach :: Vec2 -> Vec2 -> Vec2
approach v1 v2
	| (getX v1) < (getX v2) = incX v1
	| (getX v1) > (getX v2) = decX v1
	| (getY v1) < (getY v2) = incY v1
	| (getY v1) > (getY v2) = decY v1
	| otherwise = v1
