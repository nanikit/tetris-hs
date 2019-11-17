{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tetris.Update.Piece
  ( Piece
  , getPieceBlock
  , getRotatedBlockOffsets
  , getPieceOffsets
  ) where

import RIO
import RIO.List (cycle)
import RIO.List.Partial (head)
import RIO.Partial (toEnum)
import System.Random
import Tetris.Update.Board (Block(..))

data Piece = O | I | T | L | J | Z | S deriving (Enum, Eq, Show, Bounded)

instance Random Piece where
  random gen = (toEnum randInt, newGen) where
    mn = fromEnum (minBound :: Piece)
    mx = fromEnum (maxBound :: Piece)
    (randInt, newGen) = randomR (mn, mx) gen
  randomR (a, b) gen = (toEnum randInt, newGen) where
    (randInt, newGen) = randomR (fromEnum a, fromEnum b) gen

getPieceBlock :: Piece -> Block
getPieceBlock O = Red
getPieceBlock I = Orange
getPieceBlock T = Yellow
getPieceBlock L = Green
getPieceBlock J = Blue
getPieceBlock Z = Navy
getPieceBlock S = Purple

getRotatedBlockOffsets :: Piece -> [[(Int, Int)]]
getRotatedBlockOffsets = cycle . getRotatedBlockOffsets'

getRotatedBlockOffsets' :: Piece -> [[(Int, Int)]]
getRotatedBlockOffsets' O =
  [ [(0,0), (-1,0), (0,-1), (-1,-1)]
  ]
getRotatedBlockOffsets' I =
  [ [(0,0), (0,-1), (0,1), (0,2)]
  , [(0,0), (-1,0), (1,0), (2,0)]
  ]
getRotatedBlockOffsets' Z =
  [ [(0,0), (-1,0), (0,-1), (1,-1)]
  , [(0,0), (0,1), (-1,0), (-1,-1)]
  ]
getRotatedBlockOffsets' S =
  [ [(0,0), (1,0), (0,-1), (-1,-1)]
  , [(0,0), (0,-1), (-1,0), (-1,1)]
  ]
getRotatedBlockOffsets' T =
  [ [(0,0), (0,1), (-1,0), (1,0)]
  , [(0,0), (1,0), (0,1), (0,-1)]
  , [(0,0), (0,-1), (1,0), (-1,0)]
  , [(0,0), (-1,0), (0,-1), (0,1)]
  ]
getRotatedBlockOffsets' L =
  [ [(0,0), (0,1), (0,-1), (1,-1)]
  , [(0,0), (1,0), (-1,0), (-1,-1)]
  , [(0,0), (0,-1), (0,1), (-1,1)]
  , [(0,0), (-1,0), (1,0), (1,1)]
  ]
getRotatedBlockOffsets' J =
  [ [(0,0), (0,1), (0,-1), (-1,-1)]
  , [(0,0), (1,0), (-1,0), (-1,1)]
  , [(0,0), (0,-1), (0,1), (1,1)]
  , [(0,0), (-1,0), (1,0), (1,-1)]
  ]

getPieceOffsets :: Piece -> [(Int, Int)]
getPieceOffsets = head . getRotatedBlockOffsets'
