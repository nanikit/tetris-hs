{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tetris.Update
  ( update
  ) where

import RIO hiding (Right, Left, Down, drop)
import RIO.List (cycle)
import RIO.Partial (toEnum)
import System.Random
import Tetris.Update.Board
import Tetris.Update.BoardPiece

data PlayState = Intro | Playing | Pause | End

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
  isOverlap = isBlocksOverlap board (blockXys piece)

takeNextPiece :: TetrisState -> TetrisState
takeNextPiece = generateNewPiece . fixCurrentPiece

generateNewPiece :: TetrisState -> TetrisState
generateNewPiece state@TetrisState{ seed, nextPiece, consumedPieceCount } = newState where
  (newNextPiece, newSeed) = random seed
  newCurPiece :: BoardPiece = makeBoardPiece nextPiece 5 17 Zero
  newState = state
    { curPiece = newCurPiece
    , nextPiece = newNextPiece
    , consumedPieceCount = consumedPieceCount + 1
    , seed = newSeed
    }
  
fixCurrentPiece :: TetrisState -> TetrisState
fixCurrentPiece state@TetrisState{ board, score, curPiece } = newState where
  BoardPiece{ kind, blockXys } = curPiece
  block = getPieceBlock kind
  (newBoard, earnedScore) = commitBlocks board block blockXys
  newState = state
    { score = score + earnedScore
    , board = newBoard
    }
