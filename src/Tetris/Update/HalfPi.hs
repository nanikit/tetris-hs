{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tetris.Update.HalfPi where

import RIO
import RIO.Partial (succ)

data HalfPi = Zero | One | Two | Three deriving (Enum, Eq, Show)

rotateCw :: HalfPi -> HalfPi
rotateCw Three = Zero
rotateCw x = succ x

getRotation :: HalfPi -> Int
getRotation = fromEnum
