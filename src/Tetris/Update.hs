module Tetris.Update
  ( startNew,
    update,
    drop,
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
    Bounded (maxBound, minBound),
    Eq ((==)),
    Int,
    Lens',
    Maybe (Just),
    Num ((+), (-)),
    Ord ((>)),
    Show,
    Word32,
    concat,
    fromIntegral,
    id,
    length,
    over,
    subtract,
    (++),
    (.),
  )
import RIO.List (cycle, groupBy, iterate, splitAt, unfoldr)
import RIO.List.Partial (head, tail)
import RIO.Partial (toEnum)
import System.Random (Random (random, randomR), RandomGen, StdGen, mkStdGen)
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
    currentPiece :: BoardPiece,
    nextPieces :: [Piece],
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
      currentPiece = makeBoardPiece currentPiece 5 17 Zero,
      nextPieces = nextPieces,
      board = emptyBoard,
      seed = seed
    }
  where
    seed :: StdGen
    seed = mkStdGen (fromIntegral startTick)
    generate :: (RandomGen g) => g -> Maybe ([Piece], g)
    generate seed = Just (shuffle seed [minBound .. maxBound])
    currentPiece :: Piece
    currentPiece : nextPieces = concat (unfoldr generate seed)

shuffle :: (RandomGen g) => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen [x] = ([x], gen)
shuffle gen xs = (x : shuffled, lastGen)
  where
    (shuffled, lastGen) = shuffle newGen (head ++ rest)
    (index, newGen) = randomR (0, length xs - 1) gen
    (head, x : rest) = splitAt index xs

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
down prevState@TetrisState {board, currentPiece, currentTick} = nextState
  where
    downed = over y (subtract 1) currentPiece
    isTouchGround = hasOverlap board downed
    pieceMoved =
      if isTouchGround
        then takeNextPiece prevState
        else prevState {currentPiece = downed}
    nextState = pieceMoved {lastDownTick = currentTick}

left :: TetrisState -> TetrisState
left prevState@TetrisState {board, currentPiece} = nextState
  where
    moveLeft = over x (subtract 1) currentPiece
    nextState =
      if hasOverlap board moveLeft
        then prevState
        else prevState {currentPiece = moveLeft}

right :: TetrisState -> TetrisState
right prevState@TetrisState {board, currentPiece} = nextState
  where
    moveRight = over x (+ 1) currentPiece
    nextState =
      if hasOverlap board moveRight
        then prevState
        else prevState {currentPiece = moveRight}

rotate :: TetrisState -> TetrisState
rotate prevState@TetrisState {board, currentPiece} = nextState
  where
    rotated = over rotation rotateCw currentPiece
    nextState =
      if hasOverlap board rotated
        then prevState
        else prevState {currentPiece = rotated}

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
generateNewPiece state@TetrisState {seed, nextPieces, consumedPieceCount} = newState
  where
    newState =
      state
        { currentPiece = newPiece,
          nextPieces = remainingPieces,
          consumedPieceCount = consumedPieceCount + 1
        }
    nextPiece : remainingPieces = nextPieces
    newPiece :: BoardPiece = makeBoardPiece nextPiece 5 17 Zero

fixCurrentPiece :: TetrisState -> TetrisState
fixCurrentPiece state@TetrisState {board, score, currentPiece} = newState
  where
    BoardPiece {kind, blockXys} = currentPiece
    block = getPieceBlock kind
    (newBoard, earnedScore) = commitBlocks board block blockXys
    newState =
      state
        { score = score + earnedScore,
          board = newBoard
        }
