module Main where

import Vec2 as Vec2
import Monster as Mon
import UI.HSCurses.Curses
import Data.List
import Control.Monad

castEnum = toEnum . fromEnum

moveAbout :: [Mon.Monster] -> Vec2.Vec2 -> IO ()
moveAbout ms v = do
	erase

	forM_ (map pos ms) $ \(Vec2.Vec2 x y) -> (mvAddCh y x (castEnum 'x'))
	mvAddCh (getY v) (getX v) (castEnum '@')
	
	refresh

	let nms = Mon.moveMonsters ms v
	c <- getCh
	case c of
		KeyUp -> moveAbout nms $ decY v
		KeyDown -> moveAbout nms $ incY v
		KeyLeft -> moveAbout nms $ decX v
		KeyRight -> moveAbout nms $ incX v
		KeyChar '1' -> moveAbout nms $ (decX . incY) v
		KeyChar '2' -> moveAbout nms $ incY v
		KeyChar '3' -> moveAbout nms $ (incX . incY) v
		KeyChar '4' -> moveAbout nms $ decX v
		KeyChar '6' -> moveAbout nms $ incX v
		KeyChar '7' -> moveAbout nms $ (decX . decY) v
		KeyChar '8' -> moveAbout nms $ decY v
		KeyChar '9' -> moveAbout nms $ (incX . decY) v
		_	-> return ()

main = do
	initCurses
	keypad stdScr True
	echo False
	let monsters = [Mon.Monster { pos = Vec2.Vec2 1 1 }]
	cursSet CursorInvisible
	(sizeY, sizeX) <- scrSize
	moveAbout monsters (Vec2.Vec2 (sizeX `div` 2) (sizeY `div` 2))
	endWin

