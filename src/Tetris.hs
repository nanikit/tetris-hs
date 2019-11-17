{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tetris
  ( runTetris
  ) where

import RIO
import System.Random
import Tetris.Update as U
import Tetris.Render
import qualified SDL as S
import qualified SDL.Font as F

runTetris :: IO ()
runTetris = runSimpleApp tetris

tetris :: RIO SimpleApp ()
tetris = do
  context <- initDrawingContext  
  seed <- liftIO getStdGen
  let initialGameState = startNew seed
  runRIO context (eventLoop initialGameState)
  F.quit
  S.quit

eventLoop :: TetrisState -> RIO DrawingContext ()
eventLoop state = do
  render state
  event :: Maybe S.Event <- S.waitEventTimeout (1000 `quot` 10)
  case event of
    Just event -> handleEvent state event
    Nothing -> eventLoop state
  
handleEvent :: TetrisState -> S.Event -> RIO DrawingContext ()
handleEvent state event = case S.eventPayload event of
  S.WindowClosedEvent _ -> return ()
  S.KeyboardEvent{} -> do
    let cmd = getTetrisCommand event
        nextState = update state cmd
    eventLoop nextState
  _ -> eventLoop state

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