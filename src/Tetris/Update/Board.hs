{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Tetris.Update.Board
  ( Block(..)
  , Board
  , isBlocksOverlap
  , commitBlocks
  ) where

import RIO
import RIO.List (partition)
import Data.Vector.Mutable hiding (replicate, take, length)
import qualified Data.Vector.Mutable as MV
import qualified RIO.Vector.Boxed as V

data Block
  = Empty | Red | Orange | Yellow
  | Green | Blue | Navy | Purple
  deriving (Eq, Enum)

-- Origin is left bottom
type Board = V.Vector (V.Vector Block)

isBlocksOverlap :: Board -> [(Int, Int)] -> Bool
isBlocksOverlap board blocks = isOverlap where
  overlapCellsOrNothing :: Maybe [Block] =
    for blocks $ \(x, y) -> do
      inRangeRow <- board V.!? y
      inRangeCell <- inRangeRow V.!? x
      return inRangeCell
  isOverlap = case overlapCellsOrNothing of
    Nothing -> True
    Just overlapCells -> all (== Empty) overlapCells

commitBlocks :: Board -> Block -> [(Int, Int)] -> (Board, Int)
commitBlocks board block xys = (nextBoard, earnedScore) where
  committed :: [V.Vector Block] = V.toList (commit board block xys)

  isFullRow :: V.Vector Block -> Bool
  isFullRow = V.all (/= Empty)

  fullRows, holeRows :: [V.Vector Block]
  (fullRows, holeRows) = partition isFullRow committed

  earnedScore = getScoreByRowCount (length fullRows)
  nextBoard = fillVoidArea holeRows

commit :: Board -> Block -> [(Int, Int)] -> Board
commit board block xys = V.modify setBlocks board where
  setBlocks :: PrimMonad m => MVector (PrimState m) (Vector Block) -> m ()
  setBlocks rows = forM_ xys $ \(x, y) -> do
    row <- read rows y
    let newRow = V.modify (commitCell x) row
    write rows y newRow

  commitCell :: PrimMonad m => Int -> MVector (PrimState m) Block -> m ()
  commitCell x row = write row x block

fillVoidArea :: [V.Vector Block] -> Board
fillVoidArea holeRows = nextBoard where
  emptyRow :: V.Vector Block = V.replicate 10 Empty
  emptyRows :: [V.Vector Block] = replicate 20 emptyRow
  nextRows :: [V.Vector Block] = take 20 (holeRows ++ emptyRows)
  nextBoard = V.fromList nextRows

getScoreByRowCount :: Int -> Int
getScoreByRowCount 0 = 0
getScoreByRowCount 1 = 100
getScoreByRowCount 2 = 200
getScoreByRowCount 3 = 400
getScoreByRowCount 4 = 800
getScoreByRowCount _ = 0
