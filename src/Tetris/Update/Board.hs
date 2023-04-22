{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tetris.Update.Board
  ( Block (..),
    Board,
    isBlocksOverlap,
    commitBlocks,
    emptyBoard,
  )
where

import Data.Vector.Mutable (MVector, read, write)
import Data.Vector.Mutable qualified as MV
import RIO
  ( Bool (True),
    Enum,
    Eq ((/=)),
    Foldable (length),
    Int,
    Maybe (..),
    PrimMonad (PrimState),
    Show,
    Vector,
    any,
    for,
    forM_,
    replicate,
    take,
    ($),
    (++),
  )
import RIO.List (partition)
import RIO.Vector.Boxed qualified as V

data Block
  = Empty
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Navy
  | Purple
  deriving (Eq, Enum, Show)

-- Origin is left bottom
type Board = V.Vector (V.Vector Block)

isBlocksOverlap :: Board -> [(Int, Int)] -> Bool
isBlocksOverlap board blocks = isOverlap
  where
    overlapCellsOrNothing :: Maybe [Block] =
      for blocks $ \(x, y) -> do
        inRangeRow <- board V.!? y
        inRangeRow V.!? x
    isOverlap = case overlapCellsOrNothing of
      Nothing -> True
      Just overlapCells -> any (/= Empty) overlapCells

commitBlocks :: Board -> Block -> [(Int, Int)] -> (Board, Int)
commitBlocks board block xys = (nextBoard, earnedScore)
  where
    committed :: [V.Vector Block] = V.toList (commit board block xys)

    isFullRow :: V.Vector Block -> Bool
    isFullRow = V.all (/= Empty)

    fullRows, holeRows :: [V.Vector Block]
    (fullRows, holeRows) = partition isFullRow committed

    earnedScore = getScoreByRowCount (length fullRows)
    nextBoard = fillVoidArea holeRows

commit :: Board -> Block -> [(Int, Int)] -> Board
commit board block xys = V.modify setBlocks board
  where
    setBlocks :: (PrimMonad m) => MVector (PrimState m) (Vector Block) -> m ()
    setBlocks rows = forM_ xys $ \(x, y) -> do
      row <- read rows y
      let newRow = V.modify (commitCell x) row
      write rows y newRow

    commitCell :: (PrimMonad m) => Int -> MVector (PrimState m) Block -> m ()
    commitCell x row = write row x block

emptyRow :: V.Vector Block = V.replicate 10 Empty

emptyRows :: [V.Vector Block] = replicate 20 emptyRow

emptyBoard :: Board = V.fromList emptyRows

fillVoidArea :: [V.Vector Block] -> Board
fillVoidArea holeRows = nextBoard
  where
    nextRows :: [V.Vector Block] = take 20 (holeRows ++ emptyRows)
    nextBoard = V.fromList nextRows

getScoreByRowCount :: Int -> Int
getScoreByRowCount 0 = 0
getScoreByRowCount 1 = 100
getScoreByRowCount 2 = 200
getScoreByRowCount 3 = 400
getScoreByRowCount 4 = 800
getScoreByRowCount _ = 0
