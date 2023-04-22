{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tetris.Update
  ( startNew,
    update,
    Command (..),
    TetrisState (..),
    HasTetrisState (..),
    Block (..),
    Board (..),
    PlayState (..),
    BoardPiece (..),
  )
where

import RIO
  ( Bool,
    Eq ((==)),
    Int,
    Lens',
    Num ((+), (-)),
    Ord ((>)),
    Show,
    Word32,
    fromIntegral,
    id,
    over,
    subtract,
    (.),
  )
import RIO.List (cycle, groupBy, iterate)
import RIO.List.Partial (head, tail)
import RIO.Partial (toEnum)
import System.Random (Random (random), StdGen, mkStdGen)
import Tetris.Update.Board
  ( Block (..),
    Board,
    commitBlocks,
    emptyBoard,
    isBlocksOverlap,
  )
import Tetris.Update.BoardPiece
  ( BoardPiece (..),
    HalfPi (Zero),
    Piece,
    getPieceBlock,
    makeBoardPiece,
    rotateCw,
    rotation,
    x,
    y,
  )

data PlayState = Intro | Playing | Pause | End deriving (Show)

data TetrisState = TetrisState
  { startTick :: Word32,
    lastDownTick :: Word32,
    currentTick :: Word32,
    playing :: PlayState,
    consumedPieceCount :: Int,
    score :: Int,
    curPiece :: BoardPiece,
    nextPiece :: Piece,
    board :: Board,
    seed :: StdGen
  }
  deriving (Show)

class HasTetrisState env where
  stateL :: Lens' env TetrisState

startNew :: Word32 -> TetrisState
startNew startTick =
  TetrisState
    { startTick = startTick,
      lastDownTick = startTick,
      currentTick = startTick,
      playing = Intro,
      consumedPieceCount = 0,
      score = 0,
      curPiece = makeBoardPiece piece1 5 17 Zero,
      nextPiece = piece2,
      board = emptyBoard,
      seed = seed2
    }
  where
    piece1, piece2 :: Piece
    seed, seed1, seed2 :: StdGen
    seed = mkStdGen (fromIntegral startTick)
    (piece1, seed1) = random seed
    (piece2, seed2) = random seed1

data Command = Nop | Left | Right | Rotate | Down | Drop deriving (Eq)

update :: TetrisState -> Command -> TetrisState
update prevState event = nextState
  where
    action = case event of
      Nop -> id
      Down -> down
      Left -> left
      Right -> right
      Rotate -> rotate
      Drop -> drop
    nextState = downIfTimeout (action prevState)

downIfTimeout :: TetrisState -> TetrisState
downIfTimeout state@TetrisState {lastDownTick, currentTick} = nextState
  where
    action = if currentTick - lastDownTick > 1000 then down else id
    nextState = action state

down :: TetrisState -> TetrisState
down prevState@TetrisState {board, curPiece, currentTick} = nextState
  where
    downed = over y (subtract 1) curPiece
    isTouchGround = hasOverlap board downed
    pieceMoved =
      if isTouchGround
        then takeNextPiece prevState
        else prevState {curPiece = downed}
    nextState = pieceMoved {lastDownTick = currentTick}

left :: TetrisState -> TetrisState
left prevState@TetrisState {board, curPiece} = nextState
  where
    moveLeft = over x (subtract 1) curPiece
    nextState =
      if hasOverlap board moveLeft
        then prevState
        else prevState {curPiece = moveLeft}

right :: TetrisState -> TetrisState
right prevState@TetrisState {board, curPiece} = nextState
  where
    moveRight = over x (+ 1) curPiece
    nextState =
      if hasOverlap board moveRight
        then prevState
        else prevState {curPiece = moveRight}

rotate :: TetrisState -> TetrisState
rotate prevState@TetrisState {board, curPiece} = nextState
  where
    rotated = over rotation rotateCw curPiece
    nextState =
      if hasOverlap board rotated
        then prevState
        else prevState {curPiece = rotated}

drop :: TetrisState -> TetrisState
drop state = droppedAfter
  where
    futures :: [TetrisState] = iterate down state
    isConsumeSame :: TetrisState -> TetrisState -> Bool
    isConsumeSame a b = consumedPieceCount a == consumedPieceCount b
    samePieceGroups :: [[TetrisState]] = groupBy isConsumeSame futures
    droppedAfter = (head . head . tail) samePieceGroups

hasOverlap :: Board -> BoardPiece -> Bool
hasOverlap board piece = isOverlap
  where
    isOverlap = isBlocksOverlap board (blockXys piece)

takeNextPiece :: TetrisState -> TetrisState
takeNextPiece = generateNewPiece . fixCurrentPiece

generateNewPiece :: TetrisState -> TetrisState
generateNewPiece
  state@TetrisState {seed, nextPiece, consumedPieceCount} =
    newState
    where
      (newNextPiece :: Piece, newSeed) = random seed
      newCurPiece :: BoardPiece = makeBoardPiece nextPiece 5 17 Zero
      newState =
        state
          { curPiece = newCurPiece,
            nextPiece = newNextPiece,
            consumedPieceCount = consumedPieceCount + 1,
            seed = newSeed
          }

fixCurrentPiece :: TetrisState -> TetrisState
fixCurrentPiece state@TetrisState {board, score, curPiece} = newState
  where
    BoardPiece {kind, blockXys} = curPiece
    block = getPieceBlock kind
    (newBoard, earnedScore) = commitBlocks board block blockXys
    newState =
      state
        { score = score + earnedScore,
          board = newBoard
        }
