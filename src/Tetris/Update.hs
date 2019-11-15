{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tetris.Update
  ( update
  ) where

import RIO hiding (Right, Left, Down, drop)
import qualified RIO.Vector.Boxed as V
import RIO.List (cycle)
import RIO.Partial (toEnum)
import RIO.List.Partial ((!!))
import System.Random
import Tetris.Update.Piece
import Tetris.Update.HalfPi

data Block = Empty | Red | Orange | Yellow | Green | Blue | Navy | Purple deriving Eq

-- Origin is left bottom
type Board = V.Vector (V.Vector Block)

data PlayState = Intro | Playing | Pause | End

data BoardPiece = BoardPiece
  { kind :: Piece
  , x :: Int
  , y :: Int
  , rotation :: HalfPi
  }

data TetrisState = TetrisState
  { startTime :: Int
  , playing :: PlayState
  , consumedPieceCount :: Int
  , score :: Int
  , curPiece :: BoardPiece
  , nextPiece :: Piece
  , board :: Board
  , seed :: StdGen
  }

data Event = Progress | Left | Right | Rotate | Down | Drop

update :: TetrisState -> Event -> TetrisState
update prevState event = nextState where
  action = case event of
    Progress -> down
    Down -> down
    Left -> left
    Right -> right
    Rotate -> rotate
    Drop -> drop
  nextState = action prevState

down :: TetrisState -> TetrisState
down prevState@TetrisState{ board, curPiece } = nextState where
  BoardPiece{ y } = curPiece
  downed = curPiece { y = y - 1 }
  isTouchGround = hasOverlap board downed
  nextState = if isTouchGround
    then takeNextPiece prevState
    else prevState { curPiece = downed }

left :: TetrisState -> TetrisState
left prevState@TetrisState{ board, curPiece } = nextState where
  BoardPiece{ x } = curPiece
  moveLeft = curPiece { x = x - 1 }
  nextState = if hasOverlap board moveLeft
    then prevState
    else prevState { curPiece = moveLeft }

right :: TetrisState -> TetrisState
right prevState@TetrisState{ board, curPiece } = nextState where
  BoardPiece{ x } = curPiece
  moveRight = curPiece { x = x - 1 }
  nextState = if hasOverlap board moveRight
    then prevState
    else prevState { curPiece = moveRight }

rotate :: TetrisState -> TetrisState
rotate prevState@TetrisState{ board, curPiece } = nextState where
  BoardPiece{ rotation } = curPiece
  rotated = curPiece { rotation = rotateCw rotation }
  nextState = if hasOverlap board rotated
    then prevState
    else prevState { curPiece = rotated }

drop :: TetrisState -> TetrisState
drop = undefined

hasOverlap :: Board -> BoardPiece -> Bool
hasOverlap board piece = isOverlap where
  blocks :: [(Int, Int)] = pieceToBlocks piece
  isOverlap = isBlocksOverlap board blocks

pieceToBlocks :: BoardPiece -> [(Int, Int)]
pieceToBlocks BoardPiece{ kind, x, y, rotation } = blocks where
  rotateCount :: Int = getRotation rotation
  pieceRotations :: [[(Int, Int)]] = getRotatedBlockOffsets kind
  rotatedPiece :: [(Int, Int)] = pieceRotations !! rotateCount
  blocks = [(x + offsetX, y + offsetY) | (offsetX, offsetY) <- rotatedPiece]


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

takeNextPiece :: TetrisState -> TetrisState
takeNextPiece = generateNewPiece . fixCurrentPiece

generateNewPiece :: TetrisState -> TetrisState
generateNewPiece state@TetrisState{ seed, nextPiece, consumedPieceCount } = newState where
  (newNextPiece, newSeed) = random seed
  newCurPiece = BoardPiece
    { kind = nextPiece
    , x = 5
    , y = 17
    , rotation = Zero
    }
  newState = state
    { curPiece = newCurPiece
    , nextPiece = newNextPiece
    , consumedPieceCount = consumedPieceCount + 1
    , seed = newSeed
    }
  
fixCurrentPiece :: TetrisState -> TetrisState
fixCurrentPiece state = undefined
