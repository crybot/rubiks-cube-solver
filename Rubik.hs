{-# LANGUAGE InstanceSigs #-}

module Rubik where

import Control.Monad.State
import Data.List ((\\), transpose)
import Data.Maybe

-- http://www.math.harvard.edu/~jjchen/docs/Group%20Theory%20and%20the%20Rubik%27s%20Cube.pdf
-- import Data.Matrix
class Group m where
  (*) :: m -> m -> m -- closure product
  inverse :: m -> m -- inverse function
  identity :: m -- identity element

{-

data Face
  = U -- Up
  | D -- Down
  | L -- Left
  | R -- Right
  | F -- Front
  | B

data Cubie
  = Corner Face
           Face
           Face
  | Edge Face
         Face
  | Center Face

rotateFace :: Face -> Face -> Face
rotateFace face direction
  | direction == face = face
  | direction == opposite face = face
  | otherwise = U

-- Cubicle type ?
type Permutation = (Face -> Face -> Face) -> (Face -> Face -> Face)

newtype Cube = Cube
  { getPermutation :: Permutation
  }

combine :: Cube -> Cube -> Cube
combine (Cube p1) (Cube p2) = Cube (p1 . p2)

instance Group Cube where
  (*) = combine
    -- inverse :: Cube -> Cube
    -- inverse =
  identity = Cube (\x y z -> x y z)
-}
data Color
  = White
  | Yellow
  | Blue
  | Green
  | Red
  | Orange
  deriving (Show, Eq)

data Direction
  = U -- Up
  | D -- Down
  | L -- Left
  | R -- Right
  | F -- Front
  | B -- Back
  deriving (Show, Eq)

oppositeDir :: Direction -> Direction
oppositeDir U = D
oppositeDir D = U
oppositeDir L = R
oppositeDir R = L
oppositeDir F = B
oppositeDir B = F

-- Defines an arbitrary global ordering on the faces/rings/etc.
-- Every function that assumes an ordering on a cube object should refer to
-- this list
directions :: [Direction]
directions = [U, L, F, R, D, B]

middleDirections :: [Direction]
middleDirections = directions \\ [U, D]

data Cubie
  = Corner Direction
           Direction
           Direction
  | Edge Direction
         Direction
  | Facet Direction
  deriving (Show, Eq)

showCubie :: Cubie -> String
showCubie (Facet d) = "[" ++ show d ++ "]"

newtype Face = Face
  { getCubies :: [[Cubie]]
  } deriving (Show, Eq)

showFace :: Face -> String
showFace face = unlines (init faces) ++ last faces -- eliminates dangling '\n'
  where
    faces = [concatMap showCubie cs | cs <- getCubies face]

makeFace :: Int -> Direction -> Face
makeFace n dir = Face $ replicate n $ replicate n (Facet dir)

rotateFaceRight :: Face -> Face
rotateFaceRight (Face cubies) = Face . transpose . reverse $ cubies

rotateFaceLeft :: Face -> Face
rotateFaceLeft (Face cubies) = Face . reverse . transpose $ cubies

mirrorFace :: Face -> Face
mirrorFace (Face cubies) = Face $ reverse cubies

orientFace :: Direction -> Face -> Face
orientFace dir face
  -- | dir == d1 = rotateFaceRight . rotateFaceRight $ face
  -- | dir == d2 = rotateFaceRight face
  -- | dir == d4 = rotateFaceLeft face
  | dir == d6 = mirrorFace face
  | otherwise = face
  where
    [d1, d2, d3, d4, d5, d6] = directions

newtype Ring = Ring
  { ringCubies :: [[Cubie]]
  } deriving (Show, Eq)

data Layer
  = UpperLayer Face
               Ring
  | MiddleLayer Ring
  deriving (Show, Eq)

{-
data Cube = Cube
  { top, left, right, front, back, bottom :: Face
  } deriving (Show, Eq)
-}
newtype FaceCube = FaceCube
  { getFaces :: [Face]
  } deriving (Show, Eq)

orientFaces :: FaceCube -> FaceCube
orientFaces (FaceCube faces) =
  FaceCube [orientFace d face | (d, face) <- zip directions faces]

orientCube :: Direction -> FaceCube -> FaceCube
orientCube dir cube
  | dir `elem` [F, B] =
    FaceCube [f1, f2, f3, f4, f5, f6]
  | otherwise = orientFaces cube
  where
    [d1, d2, d3, d4, d5, d6] = directions
    [f1, f2, f3, f4, f5, f6] = getFaces cube

rotateCubeRight :: FaceCube -> FaceCube
rotateCubeRight cube =
  FaceCube [rotateFaceLeft f1, f6, f2, f3, rotateFaceLeft f5, f4]
  where
    [f1, f2, f3, f4, f5, f6] = getFaces cube

showFaceCube :: FaceCube -> String
showFaceCube cube = padded f1' ++ f2f3f4 ++ padded f5' ++ padded f6'
  where
    [f1, f2, f3, f4, f5, f6] = getFaces cube
    f1' = showFace f1
    f2f3f4 =
      unlines $
      zipWith3
        (\x y z -> x ++ y ++ z)
        (lines . showFace $ f2)
        (lines . showFace $ f3)
        (lines . showFace $ f4) -- Consider using ZipList
    f5' = showFace f5
    f6' = showFace f6
    pad = replicate (length . takeWhile (/= '\n') $ f1') ' '
    padded = unlines . map (pad ++) . lines

makeFaceCube :: Int -> FaceCube
makeFaceCube n = FaceCube $ map (makeFace n) directions

getFace :: Direction -> FaceCube -> Face
getFace dir (FaceCube faces) = fromJust $ lookup dir $ zip directions faces

-- UNDER DEVELOPMENT/TESTING
-- WORKS FOR R,L,U and D, BUT HAS TO BE ADJUSTED FOR OTHER ORTHOGONAL ROTATIONS
-- THE BACK FACE NEEDS CLOSER ATTENTION: WHEN TAKING RINGS ALONG U IT
-- HAS TO BE MIRRORED: BUT IT MAY WORK THE SAME
-- TO GET RINGS ABOUT FACES B AND F, WE NEED TO CYCLE THROUGH ROWS AND
-- COLUMNS, THUS PERFORMING MIRRORING FOR THE SIDE FACES
toRings :: Direction -> FaceCube -> [Ring]
toRings F cube = rings
  where
    -- oriented = orientCube dir cube
    -- oriented = orientFaces cube
    oriented = rotateCubeRight cube
    faces = map (`getFace` oriented) $ filter (`notElem` pivotFaces) directions
    pivotFaces = [L, R]
    cubies = map getCubies faces
    rings = map Ring $ transpose (map transpose cubies)

toRings dir cube = rings
  where
    -- oriented = orientCube dir cube
    -- oriented = orientFaces cube
    oriented = orientCube dir cube
    faces = map (`getFace` oriented) $ filter (`notElem` pivotFaces) directions
    pivotFaces = [dir, oppositeDir dir]
    cubies = map getCubies faces
    rings
      | dir `elem` [U, D] = map Ring $ transpose cubies
      | dir `elem` [R, L] = map Ring $ transpose (map transpose cubies)
      | otherwise = map Ring $ transpose cubies

cycleRingClockWise :: Ring -> Ring
cycleRingClockWise (Ring (hd:tl)) = Ring $ tl ++ [hd]

cycleRingCounterClockWise :: Ring -> Ring
cycleRingCounterClockWise (Ring cubies) = Ring $ last cubies : init cubies


--TODO: Define CubeOrientation as a type indicating from which side are we
--      looking at the cube
--TODO: Incorporate cube orientation into FaceCube
turnFace :: Direction -> FaceCube -> FaceCube
turnFace U cube = FaceCube [f1', f2', f3', f4', f5, f6']
  where
    [f1, f2, f3, f4, f5, f6] = getFaces cube
    rings = toRings U cube
    Ring rcubies = cycleRingClockWise $ head rings
    f1' = rotateFaceLeft f1
    pivotFaces = [U, D]
    faces = map (`getFace` cube) $ filter (`notElem` pivotFaces) directions
    [f2', f3', f4', f6'] =
      map (\(r, f:face) -> Face $ r : face) (zip rcubies (map getCubies faces))


--
--
--
--
--
{-

newtype Cube =
  Cube [Layer]
  deriving (Show, Eq)

transposeLayers :: [Layer] -> [Layer]
transposeLayers [] = []
transposeLayers (l:layers) = upperLayer : middleLayers ++ [lowerLayer]
    where
        upperLayer = UpperLayer (Face ) ()

rotateCube :: Cube -> Cube
rotateCube (Cube layers) = Cube $ transposeLayers layers


-- TODO: include orientation
ring :: Int -> Ring
ring n = Ring [replicate n (Facet d) | d <- middleDirections]

cycleRing :: Ring -> Ring
cycleRing (Ring cubies) = Ring $ last cubies : init cubies


-- TODO: n < 3 case
makeCube :: Int -> Cube
makeCube n = Cube layers
  where
    layers = upperLayer : middleLayers ++ [lowerLayer]
    upperLayer = UpperLayer (makeFace n U) (ring n)
    middleLayers = replicate (n - 2) (MiddleLayer $ ring n)
    lowerLayer = UpperLayer (makeFace n L) (ring n)

oppositeDir :: Direction -> Direction
oppositeDir U = D
oppositeDir D = U
oppositeDir L = R
oppositeDir R = L
oppositeDir F = B
oppositeDir B = F

makeTurn :: Direction -> Cube -> Cube
makeTurn U (Cube (up:layers)) = Cube $ up' : layers
    where up' = case up of
                     UpperLayer f r -> UpperLayer f (cycleRing r)
                     _ -> error "not possible"

makeTurn _ _ = error "ok... i'll do that"
{-
getFace :: Cube -> Direction -> Face
getFace c U = top c
getFace c D = bottom c
getFace c L = left c
getFace c R = right c
getFace c F = front c
getFace c B = back c

-- starting from the left, clockwise
faceRing :: Cube -> Direction -> [Cubie]
faceRing cube dir = concatMap (head . getCubies) faces
  where
    faces = map (getFace cube) (filter (`notElem` pivots) directions)
    pivots = [dir, oppositeDir dir]
    -}

-}
