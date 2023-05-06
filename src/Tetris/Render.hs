module Tetris.Render
  ( DrawingContext,
    HasDrawing (..),
    initDrawingContext,
    render,
  )
where

import Foreign.C.Types (CInt)
import Linear (V4 (..))
import RIO
  ( Bool (True),
    Int,
    IsString (fromString),
    Lens',
    Maybe (Just, Nothing),
    Monad (return),
    MonadIO,
    Num ((*), (+), (-)),
    RIO,
    Show (show),
    Text,
    Traversable (mapM),
    Word8,
    flip,
    fromIntegral,
    fromMaybe,
    mapM_,
    uncurry,
    view,
    ($),
    (++),
    (.),
  )
import RIO.List (headMaybe)
import RIO.List.Partial ((!!))
import RIO.Vector qualified as V
import SDL as S
  ( PixelFormat (RGBA8888),
    Point (P),
    Rectangle (..),
    Renderer,
    RendererConfig (rendererTargetTexture),
    Texture,
    TextureAccess (TextureAccessTarget),
    TextureInfo (TextureInfo, textureHeight, textureWidth),
    V2 (V2),
    V4,
    Window,
    WindowConfig (windowInitialSize),
    clear,
    copy,
    createRenderer,
    createTexture,
    createTextureFromSurface,
    createWindow,
    defaultRenderer,
    defaultWindow,
    destroyTexture,
    fillRect,
    freeSurface,
    initializeAll,
    present,
    queryTexture,
    rendererDrawColor,
    rendererRenderTarget,
    ($=),
  )
import SDL.Font as F (Font, blended, initialize, load)
import Tetris.Render.Constants qualified as C
import Tetris.Update
  ( Block (..),
    Board,
    BoardPiece (BoardPiece, blockXys, kind),
    HasTetrisState (..),
    TetrisState (TetrisState, board, currentPiece, nextPieces, score),
  )
import Tetris.Update.Piece
  ( Piece (O),
    getPieceBlock,
    getPieceOffsets,
  )
import System.Info (os)
import RIO.Prelude (error)

data DrawingContext = DrawingContext
  { window :: Window,
    renderer :: Renderer,
    backBuffer :: Texture,
    fonts :: [Font]
  }

class HasDrawing env where
  drawingL :: Lens' env DrawingContext

initDrawingContext :: (MonadIO m) => m DrawingContext
initDrawingContext = do
  initializeAll
  F.initialize
  let windowSize = V2 800 620
      config =
        defaultWindow
          { windowInitialSize = windowSize
          }
  window <- createWindow "haskell tetris" config
  let driverIndex = -1
  renderer <-
    createRenderer
      window
      driverIndex
      defaultRenderer
        { rendererTargetTexture = True
        }
  backBuffer <- createTexture renderer RGBA8888 TextureAccessTarget windowSize
  let face = case os of
               "darwin" -> "/Library/Fonts/Arial Unicode.ttf"
               "mingw32" -> "C:\\Windows\\Fonts\\malgun.ttf"
               _ -> error "OSes other than windows and macos are not supported yet"
  let loadMalgun = F.load face
  fonts <- mapM loadMalgun [30, 55, 70]
  return
    DrawingContext
      { window,
        renderer,
        backBuffer,
        fonts
      }

render :: (HasDrawing s, HasTetrisState s) => RIO s ()
render = withBackBuffer $ do
  drawFrame

  TetrisState {board, currentPiece, nextPieces} <- view stateL
  drawBoardBlocks board
  drawCurrentPiece currentPiece
  drawNextPiece (fromMaybe O (headMaybe nextPieces))

  drawTexts

drawFrame :: (HasDrawing s) => RIO s ()
drawFrame = do
  fillBlackAll
  thickRect C.boardLeft C.boardTop C.boardWidth C.boardHeight C.borderThickness
  thickRect
    C.previewLeft
    C.previewTop
    C.previewWidth
    C.previewHeight
    C.borderThickness

drawTexts :: (HasDrawing s, HasTetrisState s) => RIO s ()
drawTexts = do
  DrawingContext {renderer} <- view drawingL
  TetrisState {score} <- view stateL

  rendererDrawColor renderer $= C.white
  drawText "Tetris v1" 500 100 2
  let scoreText = "Score: " ++ show score
  drawText (fromString scoreText) 450 300 0

drawCurrentPiece :: (HasDrawing s) => BoardPiece -> RIO s ()
drawCurrentPiece BoardPiece {kind, blockXys} = drawing
  where
    color :: V4 Word8 = getPieceColor kind
    drawing = do
      DrawingContext {renderer} <- view drawingL
      rendererDrawColor renderer $= color
      let draw = uncurry (drawBoardBlock renderer)
      mapM_ draw blockXys

drawNextPiece :: (HasDrawing s) => Piece -> RIO s ()
drawNextPiece piece = drawing
  where
    color :: V4 Word8 = getPieceColor piece
    offsets :: [(Int, Int)] = getPieceOffsets piece
    drawing = do
      DrawingContext {renderer} <- view drawingL
      rendererDrawColor renderer $= color
      mapM_ (drawAt renderer) offsets
    drawAt renderer (x, y) = do
      let l =
            C.previewLeft
              + C.borderThickness
              + C.gap
              + (fromIntegral x + 1) * (C.side + C.gap)
          t =
            C.previewBottom
              - C.borderThickness
              - ((fromIntegral y + 2) * (C.side + C.gap))
      fillRect renderer (Just (ltwh l t C.side C.side))

getPieceColor :: Piece -> V4 Word8
getPieceColor = blockToColor . getPieceBlock

drawBoardBlock :: (MonadIO m) => Renderer -> Int -> Int -> m ()
drawBoardBlock renderer x y = drawing
  where
    originInterval = C.side + C.gap
    leftmost = C.boardLeft + C.borderThickness + C.gap
    l = leftmost + fromIntegral x * originInterval

    bottommost = C.boardBottom - C.borderThickness - originInterval
    t = bottommost - (fromIntegral y * originInterval)

    drawing = fillRect renderer (Just (ltwh l t C.side C.side))

drawBoardBlocks :: (HasDrawing s) => Board -> RIO s ()
drawBoardBlocks board = do
  DrawingContext {renderer} <- view drawingL
  flip V.imapM_ board $ \y row ->
    flip V.imapM_ row $ \x cell -> do
      rendererDrawColor renderer $= blockToColor cell
      drawBoardBlock renderer x y

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

thickRect :: (HasDrawing s) => CInt -> CInt -> CInt -> CInt -> CInt -> RIO s ()
thickRect l t w h thickness = do
  DrawingContext {renderer} <- view drawingL

  let fillRect' r = fillRect renderer (Just r)
  rendererDrawColor renderer $= C.white
  fillRect' (ltwh l t w h)

  rendererDrawColor renderer $= C.black
  let k = thickness
  fillRect' (ltwh (l + k) (t + k) (w - 2 * k) (h - 2 * k))

withBackBuffer :: (HasDrawing s, HasTetrisState s) => RIO s () -> RIO s ()
withBackBuffer paint = do
  DrawingContext {renderer, backBuffer} <- view drawingL

  let useBackBuffer = rendererRenderTarget renderer $= Just backBuffer
      resetAndCommitBackBuffer = do
        rendererRenderTarget renderer $= Nothing
        copy renderer backBuffer Nothing Nothing
        present renderer

  useBackBuffer
  paint
  resetAndCommitBackBuffer

drawText :: (HasDrawing s) => Text -> CInt -> CInt -> Int -> RIO s ()
drawText text x y size = do
  DrawingContext {renderer, fonts} <- view drawingL

  let font = fonts !! size
  surface <- blended font C.white text
  texture <- createTextureFromSurface renderer surface

  TextureInfo {textureWidth, textureHeight} <- queryTexture texture
  let srcArea = Nothing
      dstArea = Just (ltwh x y textureWidth textureHeight)
  copy renderer texture srcArea dstArea

  destroyTexture texture
  freeSurface surface

ltwh :: CInt -> CInt -> CInt -> CInt -> Rectangle CInt
ltwh left top width height = Rectangle leftTop widthHeight
  where
    leftTop = P (V2 left top)
    widthHeight = V2 width height

fillBlackAll :: (HasDrawing s) => RIO s ()
fillBlackAll = do
  DrawingContext {renderer} <- view drawingL
  rendererDrawColor renderer $= C.black
  clear renderer
