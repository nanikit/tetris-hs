module Tetris.UpdateSpec (spec) where

import RIO (($))
import RIO.Vector qualified as V (cons, fromList, replicate, snoc, take, (++))
import System.IO (print)
import Test.Hspec
import Tetris.Update (Block (Empty, Red), TetrisState (board, currentPiece, score), drop, startNew)
import Tetris.Update.Board (emptyBoard)
import Tetris.Update.BoardPiece (HalfPi (Zero), Piece (I), makeBoardPiece)

spec :: Spec
spec = do
  describe "when player fills a line" $ do
    let rightHoleLine = V.replicate 9 Red `V.snoc` Empty
        board = rightHoleLine `V.cons` V.take 19 emptyBoard
        beforeDrop =
          (startNew 0)
            { board = board,
              currentPiece = makeBoardPiece I 9 17 Zero
            }
        dropped = drop beforeDrop
    it "it should remove that line" $ do
      score dropped `shouldBe` 100