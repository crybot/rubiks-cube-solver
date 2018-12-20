module Main where

import Rubik
import Control.Monad.State

main :: IO ()
main = print $ evalState rotateTop (makeCube 3)
