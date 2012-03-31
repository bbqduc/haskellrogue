module Main where

import Vec2 as Vec2
import Monster as Mon
import UI.HSCurses.Curses
import Data.List
import Control.Monad
import System.Random

castEnum = toEnum . fromEnum

moveAbout :: RandomGen g => g -> [Mon.Monster] -> Int -> Int -> IO ()
moveAbout r ms pY pX = do
	erase

	forM (map pos ms) $ \(Vec2.Vec2 x y) -> (mvAddCh x y (castEnum 'x'))
	mvAddCh pY pX (castEnum '@')
	
	refresh

	let (nms,nr) = Mon.moveMonstersR r ms pX pY--Mon.moveMonsters ms pX pY
	c <- getCh
	case c of
		KeyUp -> moveAbout nr nms (pY - 1) pX
		KeyDown -> moveAbout nr nms (pY + 1) pX
		KeyLeft -> moveAbout nr nms pY (pX - 1)
		KeyRight -> moveAbout nr nms pY (pX + 1)
		_	-> return ()

main = do
	initCurses
	keypad stdScr True
	echo False
	let monsters = [Mon.Monster { pos = Vec2.Vec2 10 10 }]
	cursSet CursorInvisible
	(sizeY, sizeX) <- scrSize
	moveAbout (mkStdGen 1) monsters (sizeY `div` 2) (sizeX `div` 2)
	endWin

