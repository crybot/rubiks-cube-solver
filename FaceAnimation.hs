{-# LANGUAGE RecordWildCards #-}
module FaceAnimation where
import Rubik

data FaceAnimation = FaceAnimation {
    rotatingFace :: Direction,
    rotation :: Rotation,
    startAngle :: Float,
    actualAngle :: Float,
    elapsedTime :: Float,
    endAngle :: Float,
    period :: Float
}

easing :: Float -> Float -> Float -> Float -> Float -> Float
-- easing start actual end elapsed period = start + end*elapsed/period
-- quadratic easing out
-- easing start actual end elapsed period = start - end*perc*(perc-2)
-- quadratic easing in/out
easing start actual end elapsed period = if perc < 1
  then start + end / 2 * perc * perc
  else let t = perc - 1 in start - end / 2 * (t * (t - 2) - 1)
  where perc = elapsed / (period / 2)

stepAnimation :: Float -> Float -> FaceAnimation -> FaceAnimation
stepAnimation t1 t2 animation@FaceAnimation {..}
  | elapsedTime >= period = animation
  | otherwise = animation { actualAngle = newAngle, elapsedTime = elapsedTime' }
 where
  elapsedTime' = elapsedTime + (t2 - t1)
  newAngle     = easing startAngle actualAngle endAngle elapsedTime period

isAnimationOver :: FaceAnimation -> Bool
isAnimationOver FaceAnimation {..} = elapsedTime >= period

repeatAnimation :: FaceAnimation -> FaceAnimation
repeatAnimation animation
  | isAnimationOver animation = animation { elapsedTime = 0.0 }
  | otherwise                 = animation
