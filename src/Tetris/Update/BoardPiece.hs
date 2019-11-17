{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tetris.Update.BoardPiece
  ( module Tetris.Update.BoardPiece
  , module Tetris.Update.Piece
  , module Tetris.Update.HalfPi
  ) where

import RIO
import RIO.List.Partial ((!!))
import Tetris.Update.Piece
import Tetris.Update.HalfPi

data BoardPiece = BoardPiece
  { kind :: Piece
  , x :: Int
  , y :: Int
  , rotation :: HalfPi
  , blockXys :: [(Int, Int)]
  } deriving Show

makeBoardPiece :: Piece -> Int -> Int -> HalfPi -> BoardPiece
makeBoardPiece piece x y rot = BoardPiece piece x y rot xys where
  xys = getBlocks piece x y rot 

getBlocks :: Piece -> Int -> Int -> HalfPi -> [(Int, Int)]
getBlocks kind x y rotation = blocks where
  rotateCount :: Int = getRotation rotation
  pieceRotations :: [[(Int, Int)]] = getRotatedBlockOffsets kind
  rotatedPiece :: [(Int, Int)] = pieceRotations !! rotateCount
  blocks = [(x + offsetX, y + offsetY) | (offsetX, offsetY) <- rotatedPiece]
