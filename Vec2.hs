module Vec2 where

data Vec2 = Vec2 Int Int
            deriving Show

getX :: Vec2 -> Int
getX (Vec2 _ x) = x

getY :: Vec2 -> Int
getY (Vec2 y _) = y

incX :: Vec2 -> Vec2
incX (Vec2 y x) = Vec2 y (x+1)

incY :: Vec2 -> Vec2
incY (Vec2 y x) = Vec2 (y+1) x

decX :: Vec2 -> Vec2
decX (Vec2 y x) = Vec2 y (x-1)

decY :: Vec2 -> Vec2
decY (Vec2 y x) = Vec2 (y-1) x

approach :: Vec2 -> Vec2 -> Vec2
approach v1 v2
	| (getX v1) < (getX v2) && (getY v1) < (getY v2) = incX . incY $ v1
	| (getX v1) < (getX v2) && (getY v1) > (getY v2) = incX . decY $ v1
	| (getX v1) > (getX v2) && (getY v1) < (getY v2) = decX . incY $ v1
	| (getX v1) > (getX v2) && (getY v1) > (getY v2) = decX . decY $ v1
	| (getX v1) < (getX v2) = incX v1
	| (getX v1) > (getX v2) = decX v1
	| (getY v1) < (getY v2) = incY v1
	| (getY v1) > (getY v2) = decY v1
	| otherwise = v1
