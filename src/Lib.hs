module Lib where

import Data.Function (on)
import Data.List (findIndices, minimumBy, transpose)
import Data.Maybe (isJust, isNothing)

data Shape = Nought | Cross
  deriving (Read, Show, Eq)

type Cell = Maybe Shape
type Board = [Cell]

boardSize = 3

makeBoard :: String -> Board
makeBoard = map charToCell . take (boardSize * boardSize)
  where
    charToCell 'o' = Just Nought
    charToCell 'x' = Just Cross
    charToCell _   = Nothing

nextMove :: Board -> Shape -> Board
nextMove brd shp = minimumBy (compare `on` opponentScore) (nextBoards brd shp)
  where
    opponentScore brd' = scoreFor brd' (opponent shp)

isWinFor :: Board -> Shape -> Bool
isWinFor brd shp = any winningSlice allSlices
  where
    winningSlice = all (== Just shp)
    allSlices = rows brd ++ cols brd ++ diagonals brd

isLossFor :: Board -> Shape -> Bool
isLossFor brd shp = isWinFor brd (opponent shp)

isDraw :: Board -> Bool
isDraw brd = isFull brd && noWin Nought && noWin Cross
  where
    noWin = not . isWinFor brd

isFull :: Board -> Bool
isFull = all isJust

nextBoards :: Board -> Shape -> [Board]
nextBoards brd shp = map makeMove emptyIdxs
  where
    makeMove n = fillCell brd n (Just shp)
    emptyIdxs  = findIndices isNothing brd

fillCell :: Board -> Int -> Cell -> Board
fillCell brd n cell
  | n >= (boardSize * boardSize) = brd
  | otherwise = before ++ [cell] ++ (drop 1 after)
  where
    (before, after) = splitAt n brd

scoreFor :: Board -> Shape -> Int
scoreFor brd shp
  | isWinFor  brd shp = 1
  | isLossFor brd shp = -1
  | isDraw    brd     = 0
  | otherwise         = -(minimum $ map opponentScore (nextBoards brd shp))
  where
    opponentScore brd' = scoreFor brd' (opponent shp)

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

opponent :: Shape -> Shape
opponent Nought = Cross
opponent Cross  = Nought
