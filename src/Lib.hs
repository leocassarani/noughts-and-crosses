module Lib where

import Data.List (transpose)
import Data.Maybe (isJust, isNothing)

data Player = Nought | Cross
  deriving (Read, Show, Eq)

type Cell = Maybe Player
type Board = [Cell]

boardSize = 3

makeBoard :: String -> Board
makeBoard = map charToCell . take (boardSize * boardSize)
  where
    charToCell 'o' = Just Nought
    charToCell 'x' = Just Cross
    charToCell _   = Nothing

isWinFor :: Board -> Player -> Bool
isWinFor brd pl = any winningSlice allSlices
  where
    winningSlice = all (== Just pl)
    allSlices = rows brd ++ cols brd ++ diagonals brd

isLossFor :: Board -> Player -> Bool
isLossFor brd pl = isWinFor brd (opponent pl)

isDraw :: Board -> Bool
isDraw brd = isFull brd && noWin Nought && noWin Cross
  where
    noWin = not . isWinFor brd

isFull :: Board -> Bool
isFull = all isJust

nextBoards :: Board -> Player -> [Board]
nextBoards brd pl = map makeMove emptyIdxs
  where
    makeMove n = fillCell brd n (Just pl)
    emptyIdxs  = map snd $ filter (isNothing . fst) $ zip brd [0..maxIndex]
    maxIndex   = boardSize * boardSize - 1

fillCell :: Board -> Int -> Cell -> Board
fillCell brd n cell
  | n >= (boardSize * boardSize) = brd
  | otherwise = before ++ [cell] ++ (drop 1 after)
  where
    (before, after) = splitAt n brd

scoreFor :: Board -> Player -> Int
scoreFor brd pl
  | isWinFor  brd pl = 1
  | isLossFor brd pl = -1
  | isDraw    brd    = 0
  | otherwise        = -(minimum $ map opponentScore (nextBoards brd pl))
  where
    opponentScore brd' = scoreFor brd' (opponent pl)

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

opponent :: Player -> Player
opponent Nought = Cross
opponent Cross  = Nought
