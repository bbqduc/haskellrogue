module Main where

import Vec2 as Vec2
import Monster as Mon
import UI.HSCurses.Curses
import Data.List
import Control.Monad

castEnum = toEnum . fromEnum

moveAbout :: [Mon.Monster] -> Int -> Int -> IO ()
moveAbout ms pY pX = do
	erase

	forM (map pos ms) $ \(Vec2.Vec2 x y) -> (mvAddCh x y (castEnum 'x'))
	mvAddCh pY pX (castEnum '@')
	
	refresh

	let nms = Mon.moveMonsters ms pX pY
	c <- getCh
	case c of
		KeyUp -> moveAbout nms (pY - 1) pX
		KeyDown -> moveAbout nms (pY + 1) pX
		KeyLeft -> moveAbout nms pY (pX - 1)
		KeyRight -> moveAbout nms pY (pX + 1)
		_	-> return ()

main = do
	initCurses
	keypad stdScr True
	echo False
	let monsters = [Mon.Monster { pos = Vec2.Vec2 1 1 }]
	cursSet CursorInvisible
	(sizeY, sizeX) <- scrSize
	moveAbout monsters (sizeY `div` 2) (sizeX `div` 2)
	endWin

