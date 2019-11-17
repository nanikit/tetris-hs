{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tetris.Render.Constants where

import Foreign.C.Types (CInt)
import Linear (V4(..))
import RIO

gap, side, borderThickness :: CInt
gap = 4
side = 25
borderThickness = 3

boardLeft, boardTop, boardWidth, boardHeight, boardBottom :: CInt
boardLeft = 20
boardTop = 20
boardWidth  = borderThickness * 2 + (gap + side) * 10 + gap
boardHeight = borderThickness * 2 + (gap + side) * 20 + gap
boardBottom = boardTop + boardHeight

previewLeft, previewTop, previewWidth, previewHeight, previewBottom :: CInt
previewLeft = boardLeft + boardWidth - borderThickness
previewTop = boardTop
previewWidth = borderThickness * 2 + (gap + side) * 3 + gap
previewHeight = borderThickness * 2 + (gap + side) * 4 + gap
previewBottom = previewTop + previewHeight

white, black, red, orange, yellow, green, blue, navy, purple :: V4 Word8
white  = V4 255 255 255 255
black  = V4   0   0   0 255
red    = V4 255   0   0 255
orange = V4 255 128   0 255
yellow = V4 255 255   0 255
green  = V4   0 255   0 255
blue   = V4   0 255 255 255
navy   = V4   0   0 128 255
purple = V4   0 128 255 255
