{-# LANGUAGE PackageImports, OverloadedStrings #-}
module CubeGraphics (Square, faceMeshes, squareMeshes, triangle) where

import qualified Data.Map                      as Map
import qualified Data.Vector                   as V

import           LambdaCube.Linear
import           LambdaCube.GL.Mesh            as LambdaCubeGL
import           Rubik

data Square = Square V3F V3F V3F V3F

mapSquare f (Square v1 v2 v3 v4) =
  Square (fmap f v1) (fmap f v2) (fmap f v3) (fmap f v4)

reverseSquare (Square v1 v2 v3 v4) = Square v2 v1 v4 v3

n :: Int
n = 3

pad :: Float
pad = 0.00

side :: Float
side = 0.2

-- TODO: optimize concatenation by moving (++) to the front of the
-- expression for all cases
faceMeshes :: Direction -> Int -> Int -> [Mesh]
faceMeshes _   0    _    = []
faceMeshes dir rows 0    = faceMeshes dir (rows - 1) n
faceMeshes U   rows cols = cubie ++ faceMeshes U rows (cols - 1)
 where
  cubie = squareMeshes $ mapSquare (subtract 0.3) $ Square
    (V3 (side * fromIntegral (cols - 1) + pad)
        0.6
        (side * fromIntegral (rows - 1) + pad)
    ) -- x1
    (V3 (side * fromIntegral cols) 0.6 (side * fromIntegral (rows - 1) + pad))     -- x2
    (V3 (side * fromIntegral (cols - 1) + pad) 0.6 (side * fromIntegral rows)) -- x3
    (V3 (side * fromIntegral cols) 0.6 (side * fromIntegral rows)) -- x4
faceMeshes D rows cols = cubie ++ faceMeshes D rows (cols - 1)
 where
  cubie = squareMeshes $ reverseSquare . mapSquare (subtract 0.3) $ Square
    (V3 (side * fromIntegral (cols - 1) + pad)
        0.0
        (side * fromIntegral (3 - rows) + pad)
    ) -- x1
    (V3 (side * fromIntegral cols) 0.0 (side * fromIntegral (3 - rows) + pad))     -- x2
    (V3 (side * fromIntegral (cols - 1) + pad)
        0.0
        (side * fromIntegral (4 - rows))
    ) -- x3
    (V3 (side * fromIntegral cols) 0.0 (side * fromIntegral (4 - rows))) -- x4
faceMeshes F rows cols = faceMeshes F rows (cols - 1) ++ cubie
 where
  cubie = squareMeshes $ mapSquare (subtract 0.3) $ Square
    (V3 (side * fromIntegral (3 - cols) + pad)
        (side * fromIntegral (3 - rows) + pad)
        0.0
    ) -- x1
    (V3 (side * fromIntegral (4 - cols))
        (side * fromIntegral (3 - rows) + pad)
        0.0
    )     -- x2
    (V3 (side * fromIntegral (3 - cols) + pad)
        (side * fromIntegral (4 - rows))
        0.0
    ) -- x3
    (V3 (side * fromIntegral (4 - cols)) (side * fromIntegral (4 - rows)) 0.0) -- x4
faceMeshes B rows cols = cubie ++ faceMeshes B rows (cols - 1)
 where
  cubie = squareMeshes $ reverseSquare . mapSquare (subtract 0.3) $ Square
    (V3 (side * fromIntegral (cols - 1) + pad)
        (side * fromIntegral (3 - rows) + pad)
        0.6
    ) -- x1
    (V3 (side * fromIntegral (cols)) (side * fromIntegral (3 - rows) + pad) 0.6)     -- x2
    (V3 (side * fromIntegral (cols - 1) + pad)
        (side * fromIntegral (4 - rows))
        0.6
    ) -- x3
    (V3 (side * fromIntegral (cols)) (side * fromIntegral (4 - rows)) 0.6) -- x4
-- FOR SOME REASON, AFTER APPLYING A PERSPECTIVE PROJECTION MATRIX, THE
-- LEFT FACE GETS SWAPPED WITH THE RIGHT FACE, SO I HAD TO FLIP THE DIRECTION
-- OF THE X AXIS. NEEDS SOME INVESTIGATION
faceMeshes R rows cols = cubie ++ faceMeshes R rows (cols - 1)
 where
  cubie = squareMeshes $ mapSquare (subtract 0.3) $ Square
    (V3 0.0
        (side * fromIntegral (rows - 1) + pad)
        (side * fromIntegral (3 - cols) + pad)
    ) -- x1
    (V3 0.0 (side * fromIntegral rows) (side * fromIntegral (3 - cols) + pad))     -- x2
    (V3 0.0
        (side * fromIntegral (rows - 1) + pad)
        (side * fromIntegral (4 - cols))
    ) -- x3
    (V3 0.0 (side * fromIntegral rows) (side * fromIntegral (4 - cols))) -- x4
faceMeshes L rows cols = faceMeshes L rows (cols - 1) ++ cubie
 where
  cubie = squareMeshes $ reverseSquare . mapSquare (subtract 0.3) $ Square
    (V3 0.6
        (side * fromIntegral (3 - rows) + pad)
        (side * fromIntegral (3 - cols) + pad)
    ) -- x1
    (V3 0.6
        (side * fromIntegral (4 - rows))
        (side * fromIntegral (3 - cols) + pad)
    )     -- x2
    (V3 0.6
        (side * fromIntegral (3 - rows) + pad)
        (side * fromIntegral (4 - cols))
    ) -- x3
    (V3 0.6 (side * fromIntegral (4 - rows)) (side * fromIntegral (4 - cols))) -- x4


squareMeshes :: Square -> [Mesh]
squareMeshes (Square v1 v2 v3 v4) = [triangle v1 v2 v3, triangle v4 v3 v2]

-- TODO: replace squareMeshes and use indices
square :: V3F -> V3F -> V3F -> V3F -> Mesh
square v1 v2 v3 v4 = Mesh
  { mAttributes = Map.fromList
                    [("position", A_V3F $ V.fromList [v1, v2, v3, v2, v3, v4])]
  , mPrimitive  = P_Triangles
  }


triangle :: V3F -> V3F -> V3F -> Mesh
triangle v1 v2 v3 = Mesh
  { mAttributes = Map.fromList [("position", A_V3F $ V.fromList [v1, v2, v3])]
  , mPrimitive  = P_Triangles
  }
