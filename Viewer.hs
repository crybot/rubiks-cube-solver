{-# LANGUAGE RecordWildCards, PackageImports, LambdaCase, OverloadedStrings #-}
import Data.Map (Map)
import "GLFW-b"  Graphics.UI.GLFW              as GLFW
import qualified Data.Map                      as Map
import qualified Data.Vector                   as V

import           LambdaCube.GL                 as LambdaCubeGL -- renderer
import           LambdaCube.GL.Mesh            as LambdaCubeGL
import           LambdaCube.GL.Mesh (Mesh)
import           Codec.Picture                 as Juicy

import           Data.Aeson
import qualified Data.ByteString               as SB

import System.Environment
import Data.Text (unpack,Text)
import Data.List (groupBy,nub)
import Data.Maybe
import Control.Monad
import LambdaCube.Linear
import Rubik
import ScrambleParser
import Control.Monad.Extra

-- Color utility functions
rgb :: Float -> Float -> Float -> V4F
rgb r g b = V4 r g b 1.0 -- the 4th channel is alpha and is set to 1 by default

white :: V4F
white = rgb 1.0 1.0 1.0

yellow :: V4F
yellow = rgb 1.0 1.0 0.0

orange :: V4F
orange = rgb 1.0 0.6 0.0

red :: V4F
red = rgb 1.0 0.0 0.0

green :: V4F
green = rgb 0.0 1.0 0.0

blue :: V4F
blue = rgb 0.0 0.0 1.0

dirToColor :: Direction -> V4F
dirToColor U = white
dirToColor D = yellow
dirToColor L = orange
dirToColor R = red
dirToColor F = green
dirToColor B = blue

n :: Int
n = 3

data FaceObject = FaceObject {
    faceObjects :: [LambdaCubeGL.Object],
    faceDirection :: Direction
}

uploadFaceToGPU :: GLStorage -> [Mesh] -> IO [LambdaCubeGL.Object]
uploadFaceToGPU storage = mapM (
    LambdaCubeGL.uploadMeshToGPU 
    >=> 
    LambdaCubeGL.addMeshToObjectArray storage "objects" ["color"])

addFaceToScene :: Cube -> FaceObject -> IO ()
addFaceToScene (Cube fs) (FaceObject objs faceDir) =
  forM_ cubies $ \(obj, Facet cubieDir) -> do
    LambdaCubeGL.enableObject obj True
    LambdaCubeGL.updateObjectUniforms obj $ do
        "color" @= return (dirToColor cubieDir)
-- since a square mesh is composed by 2 triangles, we need to make a copy
-- of each cubie so that it matches the number of triangle meshes
  where cubies = zip objs (concatMap (\c -> [c,c]) (concat f)) 
        Face f = fs Map.! faceDir

main :: IO ()
main = do
  Just pipelineDesc <- decodeStrict <$> SB.readFile "viewer.json"

  win <- initWindow "Rubik's Cube Viewer with LambdaCube3D" 640 640
  -- setup render data
  let inputSchema = makeSchema $ do
        -- object models
        defObjectArray "objects" Triangles $ do
          "position" @: Attribute_V3F
        defUniforms $ do
          "color" @: V4F
          "angle" @: Float
          "time" @: Float

  storage <- LambdaCubeGL.allocStorage inputSchema

  -- Create a standard Rubik's cube model encapsulated in a data structure
  let cube = makeCube n
  -- Generate meshes for each face of the cube, and pair them with the its
  -- direction
  let faces = [(faceMeshes d n n, d) | d <- directions]
 
  -- Upload each generated face to the GPU memory and extract the graphics
  -- object from the IO Monad
  -- objsList :: [FaceObject]
  faceObjs <- forM faces $ \(face, faceDir) -> do
      obj <- uploadFaceToGPU storage face
      return $ FaceObject obj faceDir


  -- let scramble = "FLUBUBR'L'FBDBD'"
  let scramble = "U L R U D F"
  let permutation = parsePermutations scramble
  let cube' = applyPerm permutation cube

  mapM_ (addFaceToScene cube') faceObjs
  print cube'

  -- Allocate GL pipeline
  renderer <- LambdaCubeGL.allocRenderer pipelineDesc
  LambdaCubeGL.setStorage renderer storage >>= \case -- check schema compatibility
    Just err -> putStrLn err
    Nothing  -> loop 0.0
     where
      loop :: Float -> IO ()
      loop angle = do
        -- update graphics input
        GLFW.getWindowSize win >>= \(w, h) ->
          LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)

        let keyIsPressed k = fmap (== KeyState'Pressed) $ GLFW.getKey win k
        angle' <- do
            left <- keyIsPressed Key'Left
            right <- keyIsPressed Key'Right
            if left then return $ angle - 0.1
            else if right then return $ angle + 0.1
                 else return angle

        -- angle' <- ifM (keyIsPressed Key'Left) (return $ angle - 0.1) (return angle)

        LambdaCubeGL.updateUniforms storage $ do
          "time" @= do
            Just t <- GLFW.getTime
            return (realToFrac t :: Float)
          "angle" @= return angle'
        
        -- Cube update --
        -- Cube update --

        -- Render
        LambdaCubeGL.renderFrame renderer
        GLFW.swapBuffers win
        GLFW.pollEvents

        let keyIsPressed k = fmap (== KeyState'Pressed) $ GLFW.getKey win k
        escape <- keyIsPressed Key'Escape
        if escape then return () else loop angle'

  LambdaCubeGL.disposeRenderer renderer
  -- LambdaCubeGL.disposeStorage storage
  GLFW.destroyWindow win
  GLFW.terminate

data Square = Square V3F V3F V3F V3F

mapSquare f (Square v1 v2 v3 v4) = Square (fmap f v1) (fmap f v2) (fmap f v3) (fmap f v4) 

pad :: Float
pad = 0.01

side :: Float
side = 0.2

-- TODO: optimize concatenation by moving (++) to the front of the
-- expression for all cases
faceMeshes :: Direction -> Int -> Int -> [Mesh]
faceMeshes _ 0 _ = []
faceMeshes dir rows 0 = faceMeshes dir (rows - 1) n
faceMeshes U rows cols = cubie ++ faceMeshes U rows (cols - 1)
    where cubie = squareMeshes (V3 1.0 1.0 1.0) -- white 
            $ mapSquare (subtract 0.3) $ Square 
            (V3 (side * fromIntegral (cols-1) + pad) 0.6 (side * fromIntegral (rows-1) + pad)) -- x1
            (V3 (side * fromIntegral cols)     0.6 (side * fromIntegral (rows-1) + pad))     -- x2
            (V3 (side * fromIntegral (cols-1) + pad) 0.6 (side * fromIntegral rows)) -- x3
            (V3 (side * fromIntegral cols)     0.6 (side * fromIntegral rows)) -- x4
faceMeshes D rows cols = cubie ++ faceMeshes D rows (cols - 1)
    where cubie = squareMeshes (V3 1.0 1.0 0.0) -- yellow
            $ mapSquare (subtract 0.3) $ Square 
            (V3 (side * fromIntegral (cols-1) + pad) 0.0 (side * fromIntegral (3-rows) + pad)) -- x1
            (V3 (side * fromIntegral cols)     0.0 (side * fromIntegral (3-rows) + pad))     -- x2
            (V3 (side * fromIntegral (cols-1) + pad) 0.0 (side * fromIntegral (4-rows))) -- x3
            (V3 (side * fromIntegral cols)     0.0 (side * fromIntegral (4-rows))) -- x4
faceMeshes F rows cols = faceMeshes F rows (cols - 1) ++ cubie
    where cubie = squareMeshes (V3 0.0 1.0 0.0) -- green
            $ mapSquare (subtract 0.3) $ Square 
            (V3 (side * fromIntegral (3-cols) + pad) (side * fromIntegral (3-rows) + pad) 0.0) -- x1
            (V3 (side * fromIntegral (4-cols))     (side * fromIntegral (3-rows) + pad) 0.0)     -- x2
            (V3 (side * fromIntegral (3-cols) + pad) (side * fromIntegral (4-rows)) 0.0) -- x3
            (V3 (side * fromIntegral (4-cols))     (side * fromIntegral (4-rows)) 0.0) -- x4
faceMeshes B rows cols = cubie ++ faceMeshes B rows (cols - 1)
    where cubie = squareMeshes (V3 0.0 0.0 1.0) -- blue
            $ mapSquare (subtract 0.3) $ Square 
            (V3 (side * fromIntegral (cols-1) + pad) (side * fromIntegral (3-rows) + pad) 0.6) -- x1
            (V3 (side * fromIntegral (cols))     (side * fromIntegral (3-rows) + pad) 0.6)     -- x2
            (V3 (side * fromIntegral (cols-1) + pad) (side * fromIntegral (4-rows)) 0.6) -- x3
            (V3 (side * fromIntegral (cols))     (side * fromIntegral (4-rows)) 0.6) -- x4
-- FOR SOME REASON, AFTER APPLYING A PERSPECTIVE PROJECTION MATRIX, THE
-- LEFT FACE GETS SWAPPED WITH THE RED FACE, SO I HAD TO FLIP THE DIRECTION
-- OF THE X AXIS. NEEDS SOME INVESTIGATION
faceMeshes R rows cols = cubie ++ faceMeshes R rows (cols - 1)
    where cubie = squareMeshes (V3 1.0 0.6 0.0) -- orange
            $ mapSquare (subtract 0.3) $ Square 
            (V3 0.0 (side * fromIntegral (rows-1) + pad) (side * fromIntegral (3 - cols) + pad)) -- x1
            (V3 0.0 (side * fromIntegral rows)     (side * fromIntegral (3 - cols) + pad))     -- x2
            (V3 0.0 (side * fromIntegral (rows-1) + pad) (side * fromIntegral (4 - cols))) -- x3
            (V3 0.0 (side * fromIntegral rows)     (side * fromIntegral (4 - cols))) -- x4
faceMeshes L rows cols = faceMeshes L rows (cols - 1) ++ cubie
    where cubie = squareMeshes (V3 1.0 0.6 0.0) -- orange
            $ mapSquare (subtract 0.3) $ Square 
            (V3 0.6 (side * fromIntegral (3 - rows) + pad) (side * fromIntegral (3 - cols) + pad)) -- x1
            (V3 0.6 (side * fromIntegral (4 - rows))     (side * fromIntegral (3 - cols) + pad))     -- x2
            (V3 0.6 (side * fromIntegral (3 - rows) + pad) (side * fromIntegral (4 - cols))) -- x3
            (V3 0.6 (side * fromIntegral (4 - rows))     (side * fromIntegral (4 - cols))) -- x4
            

squareMeshes :: V3F -> Square -> [Mesh]
squareMeshes c (Square v1 v2 v3 v4) = [triangle v1 v2 v3 c , triangle v2 v3 v4 c]

triangle :: V3F -> V3F -> V3F -> V3F -> Mesh
triangle v1 v2 v3 c = Mesh
  { mAttributes =
    Map.fromList
      [ ( "position"
        , A_V3F $ V.fromList
          [v1, v2, v3]
        )
      ]
  , mPrimitive  = P_Triangles
  }

initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
  GLFW.init
  GLFW.defaultWindowHints
  mapM_
    GLFW.windowHint
    [ WindowHint'ContextVersionMajor 3
    , WindowHint'ContextVersionMinor 3
    , WindowHint'OpenGLProfile OpenGLProfile'Core
    , WindowHint'OpenGLForwardCompat True
    ]
  Just win <- GLFW.createWindow width height title Nothing Nothing
  GLFW.makeContextCurrent $ Just win
  return win
