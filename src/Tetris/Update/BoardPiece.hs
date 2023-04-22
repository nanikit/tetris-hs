{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tetris.Update.BoardPiece
  ( module Tetris.Update.Piece,
    module Tetris.Update.HalfPi,
    BoardPiece (..),
    makeBoardPiece,
    x,
    y,
    rotation,
  )
where

import RIO
import RIO.List.Partial ((!!))
import Tetris.Update.HalfPi
import Tetris.Update.Piece

data BoardPiece = BoardPiece
  { kind :: Piece,
    _x :: Int,
    _y :: Int,
    _rotation :: HalfPi,
    blockXys :: [(Int, Int)]
  }
  deriving (Show)

x :: Lens' BoardPiece Int
x = lens _x setter
  where
    setter piece val = p2
      where
        p1 = piece {_x = val}
        p2 = p1 {blockXys = getBlocks p1}

y :: Lens' BoardPiece Int
y = lens _y setter
  where
    setter piece val = p2
      where
        p1 = piece {_y = val}
        p2 = p1 {blockXys = getBlocks p1}

rotation :: Lens' BoardPiece HalfPi
rotation = lens _rotation setter
  where
    setter piece val = p2
      where
        p1 = piece {_rotation = val}
        p2 = p1 {blockXys = getBlocks p1}

makeBoardPiece :: Piece -> Int -> Int -> HalfPi -> BoardPiece
makeBoardPiece piece x y rot = boardPiece
  where
    xys = getBlocks boardPiece
    boardPiece = BoardPiece piece x y rot xys

getBlocks :: BoardPiece -> [(Int, Int)]
getBlocks BoardPiece {..} = blocks
  where
    rotateCount :: Int = getRotation _rotation
    pieceRotations :: [[(Int, Int)]] = getRotatedBlockOffsets kind
    rotatedPiece :: [(Int, Int)] = pieceRotations !! rotateCount
    blocks = [(_x + offsetX, _y + offsetY) | (offsetX, offsetY) <- rotatedPiece]
