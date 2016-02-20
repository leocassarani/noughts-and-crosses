module Main where

import Lib
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run (board:shape:_) = playGame (makeBoard board) (read shape)
run (board:_)       = playGame (makeBoard board) Nought -- Default to Nought
run []              = putStrLn "Error: not enough arguments" >> exitFailure

playGame :: Board -> Shape -> IO ()
playGame board shape
  | gameOver board = return ()
  | otherwise =
    let next = nextMove board shape
    in  prettyPrint next >> playGame next (opponent shape)

gameOver :: Board -> Bool
gameOver brd = isWinFor brd Nought || isWinFor brd Cross || isDraw brd

prettyPrint :: Board -> IO ()
prettyPrint = putStrLn . map cellToChar
  where
    cellToChar (Just Nought) = 'o'
    cellToChar (Just Cross)  = 'x'
    cellToChar Nothing       = '_'
