module Color where
import           LambdaCube.Linear

type Color = V4F

-- Color utility functions
rgb :: Float -> Float -> Float -> V4F
rgb r g b = (/ 255) <$> V4 r g b 255 -- the 4th channel is alpha and is set to 1 by default

white :: V4F
white = rgb 255 255 255

yellow :: V4F
yellow = rgb 255 213 0

orange :: V4F
orange = rgb 255 88 0

red :: V4F
red = rgb 187 15 15

green :: V4F
green = rgb 0 165 72

blue :: V4F
blue = rgb 0 70 173
