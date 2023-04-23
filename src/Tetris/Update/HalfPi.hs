module Tetris.Update.HalfPi where

import RIO (Enum (..), Eq, Int, Show)
import RIO.Partial (succ)

data HalfPi = Zero | One | Two | Three deriving (Enum, Eq, Show)

rotateCw :: HalfPi -> HalfPi
rotateCw Three = Zero
rotateCw x = succ x

getRotation :: HalfPi -> Int
getRotation = fromEnum
