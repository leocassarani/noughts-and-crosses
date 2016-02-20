module Main where

import Lib
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (str:_) -> playGame (makeBoard str) Nought
    []      -> putStrLn "not enough arguments" >> exitFailure

playGame :: Board -> Shape -> IO ()
playGame board shape
  | gameOver board = return ()
  | otherwise = do
      let next = nextMove board shape
      print next
      playGame next (opponent shape)

gameOver :: Board -> Bool
gameOver brd = isWinFor brd Nought || isWinFor brd Cross || isDraw brd
