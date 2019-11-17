{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tetris
  ( runTetris
  ) where

import RIO as R
import System.Random
import Tetris.Update as U
import Tetris.Render
import qualified SDL as S
import qualified SDL.Font as F

data TetrisApp = TetrisApp
  { logFunc :: LogFunc
  , drawing :: DrawingContext
  , state :: TetrisState
  }

instance HasLogFunc TetrisApp where
  logFuncL = lens getter setter where
    getter = logFunc
    setter app val = app { logFunc = val }

instance HasDrawing TetrisApp where
  drawingL = lens getter setter where
    getter = drawing
    setter app val = app { drawing = val }

instance HasTetrisState TetrisApp where
  stateL = lens getter setter where
    getter = state
    setter app val = app { state = val }

runTetris :: (MonadIO m, MonadUnliftIO m) => m ()
runTetris = do
  context <- initDrawingContext  
  seed <- liftIO getStdGen
  let initialGameState = startNew seed
  logOptions <- logOptionsHandle stderr False
  withLogFunc logOptions $ \lf -> do
    let app = TetrisApp
          { logFunc = lf
          , state = initialGameState
          , drawing = context
          }
    runRIO app eventLoop
  F.quit
  S.quit

eventLoop :: RIO TetrisApp ()
eventLoop = do
  render
  event :: Maybe S.Event <- S.waitEventTimeout (1000 `quot` 10)
  case event of
    Just event -> handleEvent event
    Nothing -> eventLoop
  
handleEvent :: S.Event -> RIO TetrisApp ()
handleEvent event = case S.eventPayload event of
  S.WindowClosedEvent _ -> return ()
  S.KeyboardEvent{} -> do
    state <- view stateL
    logInfo (displayShow state)
    let cmd = getTetrisCommand event
        nextState = update state cmd
    eventLoop
  _ -> eventLoop

getTetrisCommand :: S.Event -> Command
getTetrisCommand event = command where
  command = case S.eventPayload event of
    S.KeyboardEvent key | isPressEvent key ->
      mapKeyToCommand (getKeyCode key)
    _ -> Nop

isPressEvent :: S.KeyboardEventData -> Bool
isPressEvent ev = S.keyboardEventKeyMotion ev == S.Pressed

getKeyCode :: S.KeyboardEventData -> S.Keycode
getKeyCode = S.keysymKeycode . S.keyboardEventKeysym

mapKeyToCommand :: S.Keycode -> Command
mapKeyToCommand key = case key of
  S.KeycodeDown -> U.Down
  S.KeycodeUp -> Rotate
  S.KeycodeLeft -> U.Left
  S.KeycodeRight -> U.Right
  S.KeycodeSpace -> Drop
  S.KeycodeEscape -> undefined
  _ -> Nop