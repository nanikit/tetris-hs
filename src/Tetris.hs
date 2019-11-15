{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tetris
  ( runTetris
  ) where

import RIO
import SDL as S
import SDL.Font as F
import Linear (V4(..))
import Foreign.C.Types (CInt)
import RIO.List.Partial ((!!))
import Tetris.Render

runTetris :: IO ()
runTetris = runSimpleApp tetris

tetris :: RIO SimpleApp ()
tetris = do
  context <- initDrawingContext  
  runRIO context eventLoop
  F.quit
  S.quit

eventLoop :: RIO DrawingContext ()
eventLoop = do
  events :: Maybe Event <- waitEventTimeout 1000
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  
  render
  unless qPressed eventLoop
  