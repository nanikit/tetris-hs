{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( runTetris
  ) where

import RIO
import SDL as S
import SDL.Font as F
import Linear (V4(..))
import Foreign.C.Types (CInt)

runTetris :: IO ()
runTetris = runSimpleApp tetris

tetris :: RIO SimpleApp ()
tetris = do
  context <- initDrawingContext  
  runRIO context eventLoop
  F.quit
  S.quit

data DrawingContext = DrawingContext
  { window :: Window
  , renderer :: Renderer
  , font :: Font
  }

initDrawingContext :: MonadIO m => m DrawingContext
initDrawingContext = do
  initializeAll
  F.initialize
  let config = defaultWindow
        { windowInitialSize = V2 800 600
        }
  window <- createWindow "haskell tetris" config
  let driverIndex = -1
  renderer <- createRenderer window driverIndex defaultRenderer
  font <- F.load "C:\\Windows\\Fonts\\malgun.ttf" 20
  return (DrawingContext window renderer font)

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

  DrawingContext { window, renderer, font } <- ask

  rendererDrawBlendMode renderer $= BlendAlphaBlend
  fillBlackAll

  let white = V4 255 255 255 255
  rendererDrawColor renderer $= white

  rendererRenderTarget renderer $= Nothing
  -- screenTexture <- createTexture renderer RGBA8888 
  scoreSurface <- solid font white "abcd"
  scoreTexture <- createTextureFromSurface renderer scoreSurface

  let rect l t w h = drawRect renderer (Just (ltwh l t w h))
  rect 349 50 100 150

  forM_ [0..9] $ \i -> do
    let leftTop = (i + 0) * 10
        side = 10
    fillRect renderer (Just $ Rectangle (P (V2 leftTop leftTop)) (V2 side side))

  -- copy renderer scoreTexture Nothing (Just (ltwh 450 100 100 30))
  rect 50 50 300 500
  copy renderer scoreTexture Nothing (Just (ltwh 450 200 100 30))


  -- windowSurface <- getWindowSurface window
  -- surfaceBlit scoreSurface Nothing windowSurface (Just (P (V2 470 100)))
  -- updateWindowSurface window

  freeSurface scoreSurface
  destroyTexture scoreTexture

  present renderer
  unless qPressed eventLoop

ltwh :: CInt -> CInt -> CInt -> CInt -> Rectangle CInt
ltwh left top width height = Rectangle leftTop widthHeight where
  leftTop = P (V2 left top)
  widthHeight = V2 width height

fillBlackAll :: RIO DrawingContext ()
fillBlackAll = do
  DrawingContext { renderer } <- ask
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

