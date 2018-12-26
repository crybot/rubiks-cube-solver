{-# LANGUAGE InstanceSigs #-}

module Rubik where

import           Data.List                      ( (\\)
                                                , transpose
                                                )
import           Data.Maybe
import qualified Data.Map.Strict               as Map

-- http://www.math.harvard.edu/~jjchen/docs/Group%20Theory%20and%20the%20Rubik%27s%20Cube.pdf
-- import Data.Matrix
class Group m where
  (*) :: m -> m -> m -- closure product
  inverse :: m -> m -- inverse function
  identity :: m -- identity element

-- Face direction
data Direction
  = U -- Up
  | D -- Down
  | L -- Left
  | R -- Right
  | F -- Front
  | B -- Back
  deriving (Eq, Ord)

instance Show Direction where
  show U = "W"
  show D = "Y"
  show L = "G"
  show R = "B"
  show F = "R"
  show B = "O"

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
  show (Facet d) = "[" ++ show d ++ "]"

showCubies :: [Cubie] -> String
showCubies = concatMap show

newtype Face = Face
  { getCubies :: [[Cubie]]
  } deriving (Show, Eq)

showFace :: Face -> String
showFace face = unlines (init faces) ++ last faces -- eliminates dangling '\n'
  where faces = map showCubies $ getCubies face

makeFace :: Int -> Direction -> Face
{-
makeFace _ U = Face
  [ [Facet R, Facet D, Facet D]
  , [Facet D, Facet U, Facet U]
  , [Facet B, Facet L, Facet U]
  ]
makeFace _ L = Face
  [ [Facet L, Facet L, Facet R]
  , [Facet L, Facet L, Facet R]
  , [Facet R, Facet U, Facet L]
  ]

makeFace _ R = Face
  [ [Facet R, Facet R, Facet L]
  , [Facet R, Facet R, Facet L]
  , [Facet R, Facet R, Facet U]
  ]

makeFace _ D = Face
  [ [Facet D, Facet D, Facet U]
  , [Facet D, Facet D, Facet U]
  , [Facet D, Facet D, Facet D]
  ]
makeFace _ F = Face
  [ [Facet F, Facet F, Facet F]
  , [Facet B, Facet F, Facet F]
  , [Facet L, Facet D, Facet L]
  ]
makeFace _ B = Face
  [ [Facet B, Facet B, Facet B]
  , [Facet R, Facet B, Facet B]
  , [Facet U, Facet F, Facet B]
  ]
  -}
makeFace n dir = Face $ replicate n $ replicate n (Facet dir)

rotateFace :: Direction -> Rotation -> Face -> Face
-- rotateFace L r         face          = rotateFace R (reverseRotation r) face
-- rotateFace D r         face          = rotateFace U (reverseRotation r) face
rotateFace _ Clockwise (Face cubies) = Face . transpose . reverse $ cubies
rotateFace _ CounterClockwise (Face cubies) =
  Face . reverse . transpose $ cubies

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


data Layer = UpperLayer Face Ring | MiddleLayer Ring
           deriving (Show, Eq)

newtype Cube = Cube { getFaces :: Map.Map Direction Face } deriving (Eq)

instance Show Cube where
  show cube = padded f1 ++ f2f3f4 ++ padded f5 ++ padded f6
   where
    faces  = getFaces cube
    f1     = showFace $ faces Map.! U
    f2f3f4 = unlines $ zipWith3 (\x y z -> x ++ y ++ z)
                                (lines . showFace $ faces Map.! L)
                                (lines . showFace $ faces Map.! F)
                                (lines . showFace $ faces Map.! R) -- Consider using ZipList
    f5     = showFace $ faces Map.! D
    f6     = showFace $ faces Map.! B
    pad    = replicate (length . takeWhile (/= '\n') $ f1) ' '
    padded = unlines . map (pad ++) . lines

makeCube :: Int -> Cube
makeCube n = Cube $ Map.fromList $ zip directions faces
  where faces = map (makeFace n) directions

correctFaceRotation :: Axis -> Direction -> Rotation -> Rotation
correctFaceRotation X L = id
correctFaceRotation X R = reverseRotation

correctFaceRotation Y U = id
correctFaceRotation Y D = reverseRotation

correctFaceRotation Z F = id
correctFaceRotation Z B = reverseRotation

correctFaceRotation _ _ = id


rotateCube :: Axis -> Rotation -> Cube -> Cube
rotateCube Z    _        _    = undefined -- rotating about the Z axis is problematic and (maybe) not necessary
rotateCube axis rotation cube = Cube $ Map.fromList $ zip dirs faces
 where
  pivot = pivotDirections axis
  dirs  = map (rotate axis rotation) $ Map.keys (getFaces cube)
  -- rotate pivot faces about the axis of rotation
  faces =
    let mapFace (d, f)
          | d `elem` pivot = rotateFace d
                                        (correctFaceRotation axis d rotation)
                                        f
          | d == B = Face $ map reverse $ getCubies f
          | rotate axis rotation d == B = Face
          $ reverse
          $ map reverse
          $ getCubies f
          | otherwise = f
    in  map mapFace $ Map.assocs $ getFaces cube

rotateCubeClockwise :: Axis -> Cube -> Cube
rotateCubeClockwise axis = rotateCube axis Clockwise

rotateCubeCounterClockwise :: Axis -> Cube -> Cube
rotateCubeCounterClockwise axis = rotateCube axis CounterClockwise

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
{-
turnCube dir rotation cube = let mapFace d f =

                                 in Cube $ Map.mapWithKey mapFace faces
 where
  axis = getAxis dir
  pivot     = pivotDirections axis
  faces     = Map.adjust (rotateFace dir rotation) dir $ getFaces cube
  Ring ring = rotateRing axis rotation $ faceRing dir cube
  cube'     = Cube $ Map.mapWithKey
    (\d (Face f) -> case d of
      B -> Face $ init f ++ [reverse $ ring Map.! d]
      L -> Face $ ring Map.! d : tail f
      R -> Face $ reverse (ring Map.! d) : tail f
      F -> Face $ ring Map.! d : tail f
      _ -> Face f
    )
    faces
-}

{-
turnCube R Clockwise cube =
  rotateCubeCounterClockwise Y
    $ rotateCubeCounterClockwise Y
    $ turnCube L Clockwise
    $ rotateCubeClockwise Y
    $ rotateCubeClockwise Y cube
-}

turnCube U rotation cube = cube'
 where
  pivot     = pivotDirections Y
  faces     = Map.adjust (rotateFace U rotation) U $ getFaces cube
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
  pivot = pivotDirections Y
  faces =
    Map.adjust (rotateFace D (reverseRotation rotation)) D $ getFaces cube
  Ring ring = rotateRing Y rotation $ faceRing D cube
  cube' =
    let mapFace d (Face f)
          | d `elem` pivot
          = Face f
          | d == B
          = Face $ reverse (ring Map.! d) : tail f
          | rotate Y (reverseRotation rotation) d == B
          = Face $ init f ++ [reverse $ ring Map.! d]
          | otherwise
          = Face $ init f ++ [ring Map.! d]
    in  Cube $ Map.mapWithKey mapFace faces

turnCube L rotation cube = cube'
 where
  pivot     = pivotDirections X
  Ring ring = rotateRing X rotation $ faceRing L cube
  faces     = Map.adjust (rotateFace L rotation) L $ getFaces cube
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
  faces     = Map.adjust (rotateFace R rotation) R $ getFaces cube
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
  axis = getAxis F
  pivot     = pivotDirections axis
  Ring ring = rotateRing axis rotation $ faceRing F cube
  ring'     = if rotation == Clockwise then Map.map reverse ring else ring
  faces     = Map.adjust (rotateFace F rotation) F $ getFaces cube
  cube' =
    let mapFace d (Face f)
          | d `elem` pivot = Face f
          | d == U = Face $ init f ++ [ring' Map.! d] -- counter cw
          | d == R = Face $ transpose $ reverse (ring' Map.! d) : tail (transpose f)
          | d == D = Face $ (ring' Map.! d) : tail f -- counter cw
          | d == L = Face $ transpose $ init (transpose f) ++ [reverse $ ring' Map.! d]

    in  Cube $ Map.mapWithKey mapFace faces



turnCube _ _ _ = undefined


{-
orientFaces :: Cube -> Cube
orientFaces (Cube faces) =
  Cube [orientFace d face | (d, face) <- zip directions faces]

orientCube :: Direction -> Cube -> Cube
orientCube dir cube
  | dir `elem` [F, B] =
    Cube [f1, f2, f3, f4, f5, f6]
  | otherwise = orientFaces cube
  where
    [d1, d2, d3, d4, d5, d6] = directions
    [f1, f2, f3, f4, f5, f6] = getFaces cube


rotateCubeCounterClockwise :: Cube -> Axis -> Cube
rotateCubeCounterClockwise cube axis = case axis of
                                            X -> rotatedX
                                            Y -> rotatedY
                                            Z -> rotatedZ
  where
    [f1, f2, f3, f4, f5, f6] = getFaces cube
    rotatedX = Cube [rotateFaceLeft f1, f6, f2, f3, rotateFaceLeft f5, f4]
    rotatedY = Cube [rotateFaceLeft f1, f6, f2, f3, rotateFaceLeft f5, f4]
    rotatedZ = Cube [rotateFaceLeft f1, f6, f2, f3, rotateFaceLeft f5, f4]

rotateCubeRight :: Cube -> Cube
rotateCubeRight cube =
  Cube [rotateFaceLeft f1, f6, f2, f3, rotateFaceLeft f5, f4]
  where
    [f1, f2, f3, f4, f5, f6] = getFaces cube


getFace :: Direction -> Cube -> Face
getFace dir (Cube faces) = fromJust $ lookup dir $ zip directions faces

-- UNDER DEVELOPMENT/TESTING
-- WORKS FOR R,L,U and D, BUT HAS TO BE ADJUSTED FOR OTHER ORTHOGONAL ROTATIONS
-- THE BACK FACE NEEDS CLOSER ATTENTION: WHEN TAKING RINGS ALONG U IT
-- HAS TO BE MIRRORED: BUT IT MAY WORK THE SAME
-- TO GET RINGS ABOUT FACES B AND F, WE NEED TO CYCLE THROUGH ROWS AND
-- COLUMNS, THUS PERFORMING MIRRORING FOR THE SIDE FACES
toRings :: Direction -> Cube -> [Ring]
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
turnFace :: Direction -> Cube -> Cube
turnFace U cube = Cube [f1', f2', f3', f4', f5, f6']
  where
    [f1, f2, f3, f4, f5, f6] = getFaces cube
    rings = toRings U cube
    Ring rcubies = cycleRingClockWise $ head rings
    f1' = rotateFaceLeft f1
    pivotFaces = [U, D]
    faces = map (`getFace` cube) $ filter (`notElem` pivotFaces) directions
    [f2', f3', f4', f6'] =
      map (\(r, _:face) -> Face $ r : face) (zip rcubies (map getCubies faces))

-}
