{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tetris.Render
  ( DrawingContext
  , initDrawingContext
  , render
  ) where

import Foreign.C.Types (CInt)
import Linear (V4(..))
import RIO
import RIO.List.Partial ((!!))
import SDL as S
import SDL.Font as F
import Tetris.Update
import Tetris.Update.Piece
import qualified Tetris.Render.Constants as C
import qualified RIO.Vector as V

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
  fillBlackAll

  thickRect C.boardLeft C.boardTop C.boardWidth C.boardHeight C.borderThickness
  thickRect
    C.previewLeft C.previewTop C.previewWidth C.previewHeight C.borderThickness

  let TetrisState{ board, curPiece, nextPiece, score } = state
  drawBoardBlocks board

  DrawingContext { renderer } <- ask
  drawCurrentPiece curPiece
  drawNextPiece nextPiece

  -- drawHelperGrid
  
  rendererDrawColor renderer $= C.white
  drawText "Tetris v1" 500 100 2
  let scoreText = "Score: " ++ show score
  drawText (fromString scoreText) 450 300 0

drawCurrentPiece :: BoardPiece -> RIO DrawingContext ()
drawCurrentPiece BoardPiece{ kind, x, y, blockXys } = drawing where
  color :: V4 Word8 = getPieceColor kind
  absolutes = [(x + bx, y + by) | (bx, by) <- blockXys]
  drawing = do
    DrawingContext{ renderer } <- ask
    rendererDrawColor renderer $= color
    mapM_ (drawAt renderer) absolutes
  drawAt renderer (x, y) = do
    let l = C.boardLeft + C.borderThickness + C.gap
          + (fromIntegral x + 1) * (C.side + C.gap)
        t = C.boardBottom - C.borderThickness
          - ((fromIntegral y + 2) * (C.side + C.gap))
    fillRect renderer (Just (ltwh l t C.side C.side))

drawNextPiece :: Piece -> RIO DrawingContext ()
drawNextPiece piece = drawing where
  color :: V4 Word8 = getPieceColor piece
  offsets :: [(Int, Int)] = getPieceOffsets piece
  drawing = do
    DrawingContext{ renderer } <- ask
    rendererDrawColor renderer $= color
    mapM_ (drawAt renderer) offsets
  drawAt renderer (x, y) = do
    let l = C.previewLeft + C.borderThickness + C.gap
          + (fromIntegral x + 1) * (C.side + C.gap)
        t = C.previewBottom - C.borderThickness
          - ((fromIntegral y + 2) * (C.side + C.gap))
    fillRect renderer (Just (ltwh l t C.side C.side))


getPieceColor :: Piece -> V4 Word8
getPieceColor = blockToColor . getPieceBlock

drawBoardBlocks :: Board -> RIO DrawingContext ()
drawBoardBlocks board = do
  DrawingContext { renderer } <- ask
  flip V.imapM_ board $ \y row ->
    flip V.imapM_ row $ \x cell -> do
      let l = C.boardLeft + C.borderThickness + C.gap
            + fromIntegral x * (C.side + C.gap)
          t = C.boardBottom - C.borderThickness
            - ((fromIntegral y + 1) * (C.side + C.gap))
      rendererDrawColor renderer $= blockToColor cell
      fillRect renderer (Just (ltwh l t C.side C.side))

blockToColor :: Block -> V4 Word8
blockToColor block = case block of
  Empty -> C.black
  Red -> C.red
  Orange -> C.orange
  Yellow -> C.yellow
  Green -> C.green
  Blue -> C.blue
  Navy -> C.navy
  Purple -> C.purple

thickRect :: CInt -> CInt -> CInt -> CInt -> CInt -> RIO DrawingContext ()
thickRect l t w h thickness = do
  DrawingContext { renderer } <- ask

  let fillRect' r = fillRect renderer (Just r)
  rendererDrawColor renderer $= C.white
  fillRect' (ltwh l t w h)

  rendererDrawColor renderer $= C.black
  let k = thickness
  fillRect' (ltwh (l + k) (t + k) (w - 2 * k) (h - 2 * k))

drawHelperGrid :: RIO DrawingContext ()
drawHelperGrid = do
  DrawingContext { renderer } <- ask
  rendererDrawColor renderer $= C.yellow
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

  let font = fonts !! size
  surface <- blended font C.white text
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
  rendererDrawColor renderer $= C.black
  clear renderer
