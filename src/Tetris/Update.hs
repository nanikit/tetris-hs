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
import RIO.List.Partial ((!!))

data Piece = O | I | T | L | J | Z | S deriving Eq

data Block = Empty | Red | Orange | Yellow | Green | Blue | Navy | Purple deriving Eq

-- Origin is left bottom
type Board = V.Vector (V.Vector Block)

data HalfPi = Zero | One | Two | Three deriving Eq

data TetrisState = TetrisState
  { startTime :: Int
  , consumedPieceCount :: Int
  , score :: Int
  , curPiece :: Piece
  , pieceX :: Int
  , pieceY :: Int
  , pieceRotation :: HalfPi
  , nextPiece :: Piece
  , board :: Board
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
down prevState@TetrisState{ pieceY } = nextState where
  downed = prevState { pieceY = pieceY - 1 }
  nextState = if hasOverlap downed
    then takeNextPiece prevState
    else downed

left :: TetrisState -> TetrisState
left prevState@TetrisState{ pieceX } = nextState where
  moveLeft = prevState { pieceX = pieceX - 1 }
  nextState = if hasOverlap moveLeft then prevState else moveLeft

right :: TetrisState -> TetrisState
right prevState@TetrisState{ pieceX } = nextState where
  moveRight = prevState { pieceX = pieceX + 1 }
  nextState = if hasOverlap moveRight then prevState else moveRight

rotate :: TetrisState -> TetrisState
rotate prevState@TetrisState{ pieceRotation } = nextState where
  rotated = prevState { pieceRotation = rotateCw pieceRotation }
  nextState = if hasOverlap rotated then prevState else rotated

drop :: TetrisState -> TetrisState
drop = undefined

rotateCw :: HalfPi -> HalfPi
rotateCw Zero = One
rotateCw One = Two
rotateCw Two = Three
rotateCw Three = Zero

hasOverlap :: TetrisState -> Bool
hasOverlap state@TetrisState{
    board, curPiece, pieceX, pieceY, pieceRotation
  } = isOverlap where
  blocks :: [(Int, Int)] = pieceToBlocks curPiece pieceX pieceY pieceRotation
  isOverlap = isBlocksOverlap board blocks

pieceToBlocks :: Piece -> Int -> Int -> HalfPi -> [(Int, Int)]
pieceToBlocks piece pieceX pieceY pieceRotation = blocks where
  rotateCount :: Int = getRotation pieceRotation
  pieceRotations :: [[(Int, Int)]] = cycle (pieceToBlockRotations piece)
  rotatedPiece :: [(Int, Int)] = pieceRotations !! rotateCount
  blocks = [(x + pieceX, y + pieceY) | (x, y) <- rotatedPiece]

getRotation :: HalfPi -> Int
getRotation Zero = 0
getRotation One = 1
getRotation Two = 2
getRotation Three = 3

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

pieceToBlockRotations :: Piece -> [[(Int, Int)]]
pieceToBlockRotations O =
  [ [(0,0), (-1,0), (0,-1), (-1,-1)]
  ]
pieceToBlockRotations I =
  [ [(0,0), (0,-1), (0,1), (0,2)]
  , [(0,0), (-1,0), (1,0), (2,0)]
  ]
pieceToBlockRotations Z =
  [ [(0,0), (-1,0), (0,-1), (1,-1)]
  , [(0,0), (0,1), (-1,0), (-1,-1)]
  ]
pieceToBlockRotations S =
  [ [(0,0), (1,0), (0,-1), (-1,-1)]
  , [(0,0), (0,-1), (-1,0), (-1,1)]
  ]
pieceToBlockRotations T =
  [ [(0,0), (0,1), (-1,0), (1,0)]
  , [(0,0), (1,0), (0,1), (0,-1)]
  , [(0,0), (0,-1), (1,0), (-1,0)]
  , [(0,0), (-1,0), (0,-1), (0,1)]
  ]
pieceToBlockRotations L =
  [ [(0,0), (0,1), (0,-1), (1,-1)]
  , [(0,0), (1,0), (-1,0), (-1,-1)]
  , [(0,0), (0,-1), (0,1), (-1,1)]
  , [(0,0), (-1,0), (1,0), (1,1)]
  ]
pieceToBlockRotations J =
  [ [(0,0), (0,1), (0,-1), (-1,-1)]
  , [(0,0), (1,0), (-1,0), (-1,1)]
  , [(0,0), (0,-1), (0,1), (1,-1)]
  , [(0,0), (-1,0), (1,0), (-1,1)]
  ]

takeNextPiece = generateNewPiece . fixCurrentPiece

generateNewPiece = undefined

fixCurrentPiece = undefined