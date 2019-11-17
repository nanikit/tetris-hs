{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tetris.Render
  ( DrawingContext
  , initDrawingContext
  , render
  ) where

import RIO
import SDL as S
import SDL.Font as F
import Linear (V4(..))
import Foreign.C.Types (CInt)
import RIO.List.Partial ((!!))
import Tetris.Update

data DrawingContext = DrawingContext
  { window :: Window
  , renderer :: Renderer
  , backBuffer :: Texture
  , fonts :: [Font]
  }

initDrawingContext :: MonadIO m => m DrawingContext
initDrawingContext = do
  initializeAll
  F.initialize
  let windowSize = V2 800 620
      config = defaultWindow
        { windowInitialSize = windowSize
        }
  window <- createWindow "haskell tetris" config
  let driverIndex = -1
  renderer <- createRenderer window driverIndex defaultRenderer
    { rendererTargetTexture = True
    }
  backBuffer <- createTexture renderer RGBA8888 TextureAccessTarget windowSize
  let loadMalgun = F.load "C:\\Windows\\Fonts\\malgun.ttf"
  fonts <- mapM loadMalgun [30, 55, 70]
  return DrawingContext {
    window,
    renderer,
    backBuffer,
    fonts
    }

render :: TetrisState -> RIO DrawingContext ()
render state = withBackBuffer $ do
  DrawingContext { renderer } <- ask

  fillBlackAll

  thickRect 20 20 300 589 3
  thickRect 317 20 100 150 3
  let white = V4 255 255 255 255
  rendererDrawColor renderer $= white
  forM_ [0..19] $ \y -> do
    forM_ [0..9] $ \x -> do
      let l = x * (side + gap) + 23 + gap
          t = y * (side + gap) + 23 + gap
          gap = 4
          side = 25
      fillRect renderer (Just (ltwh l t side side))

  -- drawHelperGrid
  
  rendererDrawColor renderer $= white
  drawText "Tetris v1" 500 100 2
  drawText "Score: 0" 450 300 0

thickRect :: CInt -> CInt -> CInt -> CInt -> CInt -> RIO DrawingContext ()
thickRect l t w h thickness = do
  DrawingContext { renderer } <- ask

  let white = V4 255 255 255 255
      black = V4 0 0 0 255
      fillRect' r = fillRect renderer (Just r)
  rendererDrawColor renderer $= white
  fillRect' (ltwh l t w h)

  rendererDrawColor renderer $= black
  let k = thickness
  fillRect' (ltwh (l + k) (t + k) (w - 2 * k) (h - 2 * k))

drawHelperGrid :: RIO DrawingContext ()
drawHelperGrid = do
  DrawingContext { renderer } <- ask
  let yellow = V4 255 255 0 255
  rendererDrawColor renderer $= yellow
  forM_ [0..8] $ \i -> do
    let offset = i * 100
        v1 = P (V2 offset   0)
        v2 = P (V2 offset 600)
        h1 = P (V2   0 offset)
        h2 = P (V2 800 offset)
    drawLine renderer v1 v2
    drawLine renderer h1 h2

withBackBuffer :: RIO DrawingContext () -> RIO DrawingContext ()
withBackBuffer paint = do
  DrawingContext { renderer, backBuffer } <- ask

  let useBackBuffer = rendererRenderTarget renderer $= Just backBuffer
      resetAndCommitBackBuffer = do
        rendererRenderTarget renderer $= Nothing
        copy renderer backBuffer Nothing Nothing
        present renderer

  useBackBuffer
  paint
  resetAndCommitBackBuffer

drawText :: Text -> CInt -> CInt -> Int -> RIO DrawingContext ()
drawText text x y size = do
  DrawingContext { renderer, fonts } <- ask

  let white = V4 255 255 255 255
      font = fonts !! size
  surface <- blended font white text
  texture <- createTextureFromSurface renderer surface

  TextureInfo { textureWidth, textureHeight } <- queryTexture texture
  let srcArea = Nothing
      dstArea = Just (ltwh x y textureWidth textureHeight)
  copy renderer texture srcArea dstArea

  destroyTexture texture
  freeSurface surface

ltwh :: CInt -> CInt -> CInt -> CInt -> Rectangle CInt
ltwh left top width height = Rectangle leftTop widthHeight where
  leftTop = P (V2 left top)
  widthHeight = V2 width height

fillBlackAll :: RIO DrawingContext ()
fillBlackAll = do
  DrawingContext { renderer } <- ask
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
