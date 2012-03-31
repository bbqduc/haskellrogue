module Main where

import Vec2 as Vec2
import Monster as Mon
import UI.HSCurses.Curses
import Data.List
import Control.Monad
import System.Random

castEnum = toEnum . fromEnum

moveAbout :: RandomGen g => g -> [Mon.Monster] -> Vec2.Vec2 -> IO ()
moveAbout r ms v = do
	erase

	forM_ (map pos ms) $ \(Vec2.Vec2 x y) -> (mvAddCh y x (castEnum 'x'))
	mvAddCh (getY v) (getX v) (castEnum '@')
	
	refresh

	let (nms,nr) = Mon.moveMonstersR r ms v--Mon.moveMonsters ms pX pY
	c <- getCh
	case c of
		KeyUp -> moveAbout nr nms $ decY v
		KeyDown -> moveAbout nr nms $ incY v
		KeyLeft -> moveAbout nr nms $ decX v
		KeyRight -> moveAbout nr nms $ incX v
		KeyChar '1' -> moveAbout nr nms $ (decX . incY) v
		KeyChar '2' -> moveAbout nr nms $ incY v
		KeyChar '3' -> moveAbout nr nms $ (incX . incY) v
		KeyChar '4' -> moveAbout nr nms $ decX v
		KeyChar '6' -> moveAbout nr nms $ incX v
		KeyChar '7' -> moveAbout nr nms $ (decX . decY) v
		KeyChar '8' -> moveAbout nr nms $ decY v
		KeyChar '9' -> moveAbout nr nms $ (incX . decY) v
		_	-> return ()

main = do
	initCurses
	keypad stdScr True
	echo False
	let monsters = [Mon.Monster { pos = Vec2.Vec2 10 10 }]
	cursSet CursorInvisible
	(sizeY, sizeX) <- scrSize
	moveAbout (mkStdGen 1) monsters (Vec2.Vec2 (sizeX `div` 2) (sizeY `div` 2))
	endWin

