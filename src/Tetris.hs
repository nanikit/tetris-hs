{-# LANGUAGE CPP #-}

module Tetris
  ( runTetris,
  )
where

import RIO as R
  ( Bool,
    Eq ((/=), (==)),
    HasLogFunc (..),
    Integral (quot),
    LogFunc,
    Maybe (..),
    Monad (return),
    MonadIO (..),
    MonadReader (ask),
    MonadUnliftIO,
    RIO,
    displayShow,
    lens,
    logInfo,
    mkLogFunc,
    runRIO,
    undefined,
    view,
    when,
    ($),
    (.),
  )
import SDL qualified as S
import SDL.Font qualified as F
import System.Random
import Tetris.Render
  ( DrawingContext,
    HasDrawing (..),
    initDrawingContext,
    render,
  )
import Tetris.Update
  ( Command (Drop, Nop, Rotate),
    HasTetrisState (..),
    TetrisState (currentTick),
    startNew,
  )
import Tetris.Update qualified as U
import Prelude (putStrLn)

data TetrisApp = TetrisApp
  { logFunc :: LogFunc,
    drawing :: DrawingContext,
    state :: TetrisState
  }

instance HasLogFunc TetrisApp where
  logFuncL = lens getter setter
    where
      getter = logFunc
      setter app val = app {logFunc = val}

instance HasDrawing TetrisApp where
  drawingL = lens getter setter
    where
      getter = drawing
      setter app val = app {drawing = val}

instance HasTetrisState TetrisApp where
  stateL = lens getter setter
    where
      getter = state
      setter app val = app {state = val}

runTetris :: (MonadIO m, MonadUnliftIO m) => m ()
runTetris = do
  context <- initDrawingContext
  startTick <- S.ticks
  let initialGameState = startNew startTick

  liftIO $ putStrLn "release"
  let lf = mkLogFunc nopLogger
      nopLogger _ _ _ _ = return ()
      app =
        TetrisApp
          { logFunc = lf,
            state = initialGameState,
            drawing = context
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
    Nothing -> update Nop

handleEvent :: S.Event -> RIO TetrisApp ()
handleEvent event = case S.eventPayload event of
  S.WindowClosedEvent _ -> return ()
  S.KeyboardEvent {} -> do
    let cmd = getTetrisCommand event
    update cmd
  _ -> eventLoop

update :: Command -> RIO TetrisApp ()
update cmd = do
  state <- view stateL
  app <- ask
  tick <- S.ticks
  let curState = state {currentTick = tick}
      nextState = U.update curState cmd
      newApp = app {state = nextState}
  when (cmd /= Nop) $ logInfo (displayShow nextState)
  runRIO newApp eventLoop

getTetrisCommand :: S.Event -> Command
getTetrisCommand event = command
  where
    command = case S.eventPayload event of
      S.KeyboardEvent key
        | isPressEvent key ->
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
