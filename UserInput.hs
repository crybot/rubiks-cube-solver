{-# LANGUAGE PackageImports #-}
module UserInput where

import "GLFW-b"  Graphics.UI.GLFW              as GLFW
import           Control.Monad.Extra

data UserInput = UserInput {
  left :: Bool,
  right :: Bool,
  up :: Bool,
  down :: Bool
}

getUserInput :: Window -> IO UserInput
getUserInput win =
  UserInput
    <$> pressed Key'Left
    <*> pressed Key'Right
    <*> pressed Key'Up
    <*> pressed Key'Down
  where pressed k = (== KeyState'Pressed) <$> GLFW.getKey win k
