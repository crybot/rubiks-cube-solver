{-# LANGUAGE InstanceSigs #-}

module Rubik where

import           Data.List                      ( transpose )
import qualified Data.Map.Strict               as Map

-- Face direction
data Direction
  = U -- Up
    | D -- Down
    | L -- Left
    | R -- Right
    | F -- Front
    | B -- Back
  deriving (Eq, Ord, Show, Read)

readDir :: Char -> Direction
readDir = read . (: [])

encodeColor :: Direction -> String
encodeColor U = "W"
encodeColor D = "Y"
encodeColor L = "O"
encodeColor R = "R"
encodeColor F = "G"
encodeColor B = "B"

oppositeDir :: Direction -> Direction
oppositeDir U = D
oppositeDir D = U
oppositeDir L = R
oppositeDir R = L
oppositeDir F = B
oppositeDir B = F

-- Axis of rotation
data Axis = X | Y | Z

data Rotation = Clockwise | CounterClockwise deriving (Eq)

reverseRotation :: Rotation -> Rotation
reverseRotation Clockwise        = CounterClockwise
reverseRotation CounterClockwise = Clockwise

rotate :: Axis -> Rotation -> Direction -> Direction
rotate X    Clockwise        U   = F
rotate X    Clockwise        D   = B
rotate X    Clockwise        F   = D
rotate X    Clockwise        B   = U

rotate Y    Clockwise        L   = B
rotate Y    Clockwise        R   = F
rotate Y    Clockwise        F   = L
rotate Y    Clockwise        B   = R

rotate Z    Clockwise        U   = R
rotate Z    Clockwise        D   = L
rotate Z    Clockwise        L   = U
rotate Z    Clockwise        R   = D

-- pivot faces do not change direction while rotating about their axis
rotate _    Clockwise        dir = dir

-- rotating counter-clockwise is equivalent to rotating clockwise 3 times
rotate axis CounterClockwise dir = iterate (rotate axis Clockwise) dir !! 3

directions :: [Direction]
directions = [U, L, F, R, D, B]

newtype Cubie = Facet Direction
  deriving (Eq)

instance Show Cubie where
  show (Facet d) = "[" ++ encodeColor d ++ "]"

showCubies :: [Cubie] -> String
showCubies = concatMap show

newtype Face = Face
  { getCubies :: [[Cubie]]
  } deriving (Eq)

instance Show Face where
  show face = unlines (init faces) ++ last faces -- eliminates dangling '\n'
    where faces = map showCubies $ getCubies face

makeFace :: Int -> Direction -> Face
makeFace n dir = Face $ replicate n $ replicate n (Facet dir)

rotateFace :: Rotation -> Face -> Face
rotateFace Clockwise        (Face cubies) = Face . transpose . reverse $ cubies
rotateFace CounterClockwise (Face cubies) = Face . reverse . transpose $ cubies

mirrorFace :: Face -> Face
mirrorFace (Face cubies) = Face $ reverse cubies

mirrorRotateFace :: Rotation -> Face -> Face
mirrorRotateFace rotation = mirrorFace . rotateFace rotation . mirrorFace

-- get directions that spin (but don't move) about the given axis
pivotDirections :: Axis -> [Direction]
pivotDirections X = [L, R]
pivotDirections Y = [U, D]
pivotDirections Z = [F, B]

newtype Ring = Ring { ringCubies :: Map.Map Direction [Cubie] } deriving (Eq)

instance Show Ring where
  show (Ring ring) = unwords
    (map (\(d, r) -> show d ++ ':' : showCubies r) assocs)
    where assocs = Map.assocs ring

rotateRing :: Axis -> Rotation -> Ring -> Ring
rotateRing axis rotation (Ring ring) =
  Ring $ Map.mapKeys (rotate axis rotation) ring

rotateRingClockwise :: Axis -> Ring -> Ring
rotateRingClockwise axis = rotateRing axis Clockwise

rotateRingCounterClockwise :: Axis -> Ring -> Ring
rotateRingCounterClockwise axis = rotateRing axis CounterClockwise


newtype Cube = Cube { getFaces :: Map.Map Direction Face } deriving (Eq)

instance Show Cube where
  show cube = padded f1 ++ f2f3f4 ++ padded f5 ++ padded f6
   where
    faces  = getFaces cube
    f1     = show $ faces Map.! U
    f2f3f4 = unlines $ zipWith3 (\x y z -> x ++ y ++ z)
                                (lines . show $ faces Map.! L)
                                (lines . show $ faces Map.! F)
                                (lines . show $ faces Map.! R) -- Consider using ZipList
    f5     = show $ faces Map.! D
    f6     = show $ faces Map.! B
    pad    = replicate (length . takeWhile (/= '\n') $ f1) ' '
    padded = unlines . map (pad ++) . lines

makeCube :: Int -> Cube
makeCube n = Cube $ Map.fromList $ zip directions faces
  where faces = map (makeFace n) directions

-- Returns the ring of the cube associated with the given face direction.
-- This function assumes the cube is in the original orientation.
-- TODO: refactor and generalize
faceRing :: Direction -> Cube -> Ring
faceRing F cube = ring
 where
  pivot = pivotDirections Z
  faces = Map.filterWithKey (\d _ -> d `notElem` pivot) $ getFaces cube
  ring  = Ring $ Map.mapWithKey
    (\d (Face f) -> case d of
      U -> last f
      L -> map last f
      D -> head f
      R -> map head f
      _ -> undefined
    )
    faces

faceRing B cube = ring
 where
  pivot = pivotDirections Z
  faces = Map.filterWithKey (\d _ -> d `notElem` pivot) $ getFaces cube
  ring  = Ring $ Map.mapWithKey
    (\d (Face f) -> case d of
      U -> head f
      L -> map head f
      D -> last f
      R -> map last f
      _ -> undefined
    )
    faces

faceRing L cube = ring
 where
  pivot = pivotDirections X
  faces = Map.filterWithKey (\d _ -> d `notElem` pivot) $ getFaces cube
  ring  = Ring $ Map.mapWithKey
    (\d (Face f) -> if d == B then reverse $ map head f else map head f)
    faces

faceRing R cube = ring
 where
  pivot = pivotDirections X
  faces = Map.filterWithKey (\d _ -> d `notElem` pivot) $ getFaces cube
  ring  = Ring $ Map.mapWithKey
    (\d (Face f) -> if d == B then reverse $ map last f else map last f)
    faces

faceRing D cube = ring
 where
  pivot = pivotDirections Y
  faces = Map.filterWithKey (\d _ -> d `notElem` pivot) $ getFaces cube
  ring  = Ring
    $ Map.mapWithKey (\d (Face f) -> if d == B then head f else last f) faces

faceRing U cube = ring
 where
  pivot = pivotDirections Y
  faces = Map.filterWithKey (\d _ -> d `notElem` pivot) $ getFaces cube
  ring  = Ring
    $ Map.mapWithKey (\d (Face f) -> if d == B then last f else head f) faces

getAxis :: Direction -> Axis
getAxis dir | dir `elem` [L, R] = X
            | dir `elem` [U, D] = Y
            | dir `elem` [F, B] = Z
            | otherwise         = undefined

turnCube :: Direction -> Rotation -> Cube -> Cube
turnCube U rotation cube = cube'
 where
  pivot     = pivotDirections Y
  faces     = Map.adjust (rotateFace rotation) U $ getFaces cube
  Ring ring = rotateRing Y rotation $ faceRing U cube
  cube' =
    let mapFace d (Face f)
          | d `elem` pivot
          = Face f
          | d == B
          = Face $ init f ++ [reverse $ ring Map.! d]
          | rotate Y (reverseRotation rotation) d == B
          = Face $ reverse (ring Map.! d) : tail f
          | otherwise
          = Face $ ring Map.! d : tail f
    in  Cube $ Map.mapWithKey mapFace faces

-- Because the lower side rotates in the opposite direction with respect to the upper side
-- We have to rotate the ring in the opposite direction (i.e. reverseRotation rotation)
turnCube D rotation cube = cube'
 where
  pivot     = pivotDirections Y
  rotation' = reverseRotation rotation
  faces = Map.adjust (rotateFace (reverseRotation rotation')) D $ getFaces cube
  Ring ring = rotateRing Y rotation' $ faceRing D cube
  cube' =
    let mapFace d (Face f)
          | d `elem` pivot
          = Face f
          | d == B
          = Face $ reverse (ring Map.! d) : tail f
          | rotate Y (reverseRotation rotation') d == B
          = Face $ init f ++ [reverse $ ring Map.! d]
          | otherwise
          = Face $ init f ++ [ring Map.! d]
    in  Cube $ Map.mapWithKey mapFace faces

turnCube L rotation cube = cube'
 where
  pivot     = pivotDirections X
  Ring ring = rotateRing X rotation $ faceRing L cube
  faces     = Map.adjust (rotateFace rotation) L $ getFaces cube
  cube' =
    let mapFace d (Face f)
          | d `elem` pivot
          = Face f
          | rotate X (reverseRotation rotation) d == B
          = Face $ transpose $ reverse (ring Map.! d) : tail (transpose f)
          | otherwise
          = Face $ transpose $ (ring Map.! d) : tail (transpose f)
    in  Cube $ Map.mapWithKey mapFace faces

-- Because the right side rotates in the opposite direction with respect to the left side
-- We have to rotate the ring in the opposite direction (i.e. reverseRotation rotation)
turnCube R rotation cube = cube'
 where
  pivot     = pivotDirections X
  Ring ring = rotateRing X (reverseRotation rotation) $ faceRing R cube
  faces     = Map.adjust (rotateFace rotation) R $ getFaces cube
  cube' =
    let mapFace d (Face f)
          | d `elem` pivot
          = Face f
          | rotate X rotation d == B
          = Face $ transpose $ init (transpose f) ++ [reverse $ ring Map.! d]
          | otherwise
          = Face $ transpose $ init (transpose f) ++ [ring Map.! d]
    in  Cube $ Map.mapWithKey mapFace faces

turnCube F rotation cube = cube'
 where
  axis      = getAxis F
  pivot     = pivotDirections axis
  Ring ring = rotateRing axis rotation $ faceRing F cube
  ring'     = if rotation == Clockwise then Map.map reverse ring else ring
  faces     = Map.adjust (rotateFace rotation) F $ getFaces cube
  cube' =
    let mapFace d (Face f)
          | d `elem` pivot
          = Face f
          | d == U
          = Face $ init f ++ [ring' Map.! d]
          | d == R
          = Face $ transpose $ reverse (ring' Map.! d) : tail (transpose f)
          | d == D
          = Face $ (ring' Map.! d) : tail f
          | d == L
          = Face $ transpose $ init (transpose f) ++ [reverse $ ring' Map.! d]
    in  Cube $ Map.mapWithKey mapFace faces

turnCube B rotation cube = cube'
 where
  rotation' = reverseRotation rotation
  axis      = getAxis B
  pivot     = pivotDirections axis
  Ring ring = rotateRing axis rotation' $ faceRing B cube
  ring'     = if rotation' == Clockwise then Map.map reverse ring else ring
  faces     = Map.adjust (mirrorRotateFace rotation') B $ getFaces cube
  cube' =
    let mapFace d (Face f)
          | d `elem` pivot
          = Face f
          | d == U
          = Face $ (ring' Map.! d) : tail f
          | d == R
          = Face $ transpose $ init (transpose f) ++ [reverse $ ring' Map.! d]
          | d == D
          = Face $ init f ++ [ring' Map.! d]
          | d == L
          = Face $ transpose $ reverse (ring' Map.! d) : tail (transpose f)
    in  Cube $ Map.mapWithKey mapFace faces

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

newtype Permutation = Permutation { applyPerm :: Cube -> Cube }

instance Monoid Permutation where
  mempty = Permutation id
  mappend (Permutation p1) (Permutation p2) = Permutation $ p2 . p1

instance Semigroup Permutation where
  (<>) = mappend
