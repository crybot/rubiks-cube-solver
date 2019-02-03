{-# LANGUAGE PackageImports #-}
module UserInput where

import "GLFW-b"  Graphics.UI.GLFW              as GLFW
import           Control.Monad.Extra

data UserInput = UserInput {
  pressedLeft :: Bool,
  pressedRight :: Bool,
  pressedUp :: Bool,
  pressedDown :: Bool,
  pressedP :: Bool,
  pressedEsc :: Bool
}

getUserInput :: Window -> IO UserInput
getUserInput win = do
  GLFW.pollEvents
  UserInput
    <$> pressed Key'Left
    <*> pressed Key'Right
    <*> pressed Key'Up
    <*> pressed Key'Down
    <*> pressed Key'P
    <*> pressed Key'Escape
  where pressed k = (== KeyState'Pressed) <$> GLFW.getKey win k
