module Lib where

import Data.List (transpose)

data Player = Nought | Cross
  deriving (Read, Show, Eq)

type Cell = Maybe Player
type Board = [Cell]

boardSize = 3

isWinFor :: Player -> Board -> Bool
isWinFor pl brd = any winningSlice allSlices
  where
    winningSlice = all (== Just pl)
    allSlices = rows brd ++ cols brd ++ diagonals brd

rows :: Board -> [[Cell]]
rows brd = map row [0..boardSize-1]
  where
    row n = take boardSize . drop (n * boardSize) $ brd

cols :: Board -> [[Cell]]
cols = transpose . rows

diagonals :: Board -> [[Cell]]
diagonals brd = map extract [topLeft, topRight]
  where
    extract  = map (brd !!)
    topLeft  = map (\n -> n * (boardSize + 1))       [0..boardSize-1]
    topRight = map (\n -> (n + 1) * (boardSize - 1)) [0..boardSize-1]

makeBoard :: String -> Board
makeBoard = map charToCell . take (boardSize * boardSize)
  where
    charToCell 'o' = Just Nought
    charToCell 'x' = Just Cross
    charToCell _   = Nothing
