{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( runTetris
  ) where

import RIO
import SDL
import Linear (V4(..))

runTetris :: IO ()
runTetris = runSimpleApp tetris

tetris :: RIO SimpleApp ()
tetris = do
  renderer <- getRenderer
  appLoop renderer

getRenderer :: MonadIO m => m Renderer
getRenderer = do
  initializeAll
  window <- createWindow "haskell tetris" defaultWindow
  let driverIndex = -1
  createRenderer window driverIndex defaultRenderer

appLoop :: Renderer -> RIO s ()
appLoop renderer = do
  events :: Maybe Event <- waitEventTimeout 1000
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer)