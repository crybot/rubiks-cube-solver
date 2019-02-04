{-# LANGUAGE RecordWildCards, PackageImports, LambdaCase, OverloadedStrings #-}
import "GLFW-b"  Graphics.UI.GLFW              as GLFW
import qualified Data.Map                      as Map

import           LambdaCube.GL                 as LambdaCubeGL -- renderer
import           LambdaCube.GL.Mesh            as LambdaCubeGL
import           LambdaCube.GL.Input
import           LambdaCube.Linear

import           Data.Aeson
import qualified Data.ByteString               as SB
import           Data.List                      ( elemIndex )
import           Data.Maybe

import           ScrambleParser
import           FaceAnimation
import           CubeGraphics
import           UserInput
import           Color
import           Rubik

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Concurrent

data FaceObject = FaceObject {
    faceObjects :: [LambdaCubeGL.Object],
    faceDirection :: Direction
}

uploadFaceToGPU :: GLStorage -> [Mesh] -> IO [LambdaCubeGL.Object]
uploadFaceToGPU storage = mapM
  (   LambdaCubeGL.uploadMeshToGPU
  >=> LambdaCubeGL.addMeshToObjectArray storage "objects" ["color"]
  )

-- Since a square mesh is composed by 2 triangles, we need to make a copy
-- of each cubie so that it matches the number of triangle meshes
addFaceToScene :: Cube -> FaceObject -> IO ()
addFaceToScene (Cube fs) (FaceObject objs faceDir) =
  forM_ cubies $ \(obj, Facet cubieDir) -> do
    LambdaCubeGL.enableObject obj True
    LambdaCubeGL.updateObjectUniforms obj $ do
      "color" @= return (dirToColor cubieDir)
 where
  cubies = zip objs (concatMap (\c -> [c, c]) (concat f))
  Face f = fs Map.! faceDir

dirToColor :: Direction -> V4F
dirToColor U = white
dirToColor D = yellow
dirToColor L = orange
dirToColor R = red
dirToColor F = green
dirToColor B = blue

getTimeF :: IO Float
getTimeF = do
  Just t <- GLFW.getTime
  return $ realToFrac t

-- Graphics' Environment which is passed to the the rendering functions
data Env = Env { window :: Window,
                 renderer :: GLRenderer,
                 storage :: GLStorage,
                 faceObjs :: [FaceObject]}

-- World's state
data World = World { cube :: Cube,
                     angleX :: Float,
                     angleY :: Float,
                     perms :: [String],
                     animation :: FaceAnimation,
                     time :: Float
                   }

data IterationState = IterationState {
                      paused :: Bool,
                      userInput :: UserInput
                                     }

-- GraphicContext is the Monad in which the render loop performs.
-- It holds a readonly rendering environment and a boolean state that describes
-- whether the game is paused or not
type GraphicContext m = ReaderT Env (StateT IterationState m)

runGraphicContext :: (Monad m) => GraphicContext m t -> Env -> m t
runGraphicContext context env =
  evalStateT (runReaderT context env) (IterationState False emptyInput)

render :: Window -> GLRenderer -> IO ()
render win renderer = do
  LambdaCubeGL.renderFrame renderer
  GLFW.swapBuffers win

-- Decide whether we need to put/resume on/from pause the loop
iterationState :: UserInput -> IterationState -> IterationState
iterationState newInput (IterationState paused lastInput) = if paused
  then IterationState (not paused') newInput
  else IterationState paused' newInput
  where paused' = not (pressedP newInput) && pressedP lastInput

renderLoop' :: World -> GraphicContext IO ()
renderLoop' world = do
  Env {..} <- ask -- Retrieve Graphic Environment
  setupWindow -- Window Setup
  drawCube world
  paused              <- gets paused
  (world', userInput) <- liftIO $ do -- World Update
    time                     <- getTimeF
    userInput@UserInput {..} <- getUserInput window
    if paused
      then return $ (world { time = time }, userInput) -- Update time even when paused 
      else return $ (updateWorld world time userInput, userInput)

  modify (iterationState userInput)
  unless (pressedEsc userInput) (renderLoop' world')

setupWindow :: GraphicContext IO ()
setupWindow = do
  Env {..} <- ask
  liftIO $ GLFW.getWindowSize window >>= \(w, h) ->
    LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)

updateWorld :: World -> Float -> UserInput -> World
updateWorld world@World {..} time' UserInput {..} = world
  { cube      = cube'
  , angleX    = angleX'
  , angleY    = angleY'
  , animation = repeatAnimation animation'
  , time      = time'
  , perms     = perms'
  }
 where
  angleX' | pressedUp   = angleX - 0.1
          | pressedDown = angleX + 0.1
          | otherwise   = angleX
  angleY' | pressedRight = angleY + 0.1
          | pressedLeft  = angleY - 0.1
          | otherwise    = angleY
  period' | -- | pressedUp        = max (period animation - 0.00) 0.1
          -- | pressedDown      = min (period animation + 0.00) 4
            otherwise =
    period animation
  perms' | isAnimationOver animation' = tail perms ++ [head perms]
         | otherwise                  = perms
  animation' = (stepAnimation time time' animation) { period = period'
                                                    , rotatingFace = rotatingFace'
                                                    , rotation = rotation'
                                                    }
  cube' | isAnimationOver animation' = applyPerm perm cube
        | otherwise                  = cube
  (perm, rotatingFace', rotation') = parseTurn (head perms)


drawCube :: World -> GraphicContext IO ()
drawCube world@World {..} = do
  Env {..} <- ask
  liftIO $ do
  -- Update Uniform values
    LambdaCubeGL.updateUniforms storage $ do
      "time" @= return (time :: Float)
      "angleX" @= return angleX
      "angleY" @= return angleY
      "rotatingFace" @= do
        let f = fromMaybe (-1) (rotatingFace `elemIndex` directions)
        return (fromIntegral f :: Float)
      "faceAngle" @= return
        (case rotation of
          Clockwise        -> actualAngle :: Float
          CounterClockwise -> -actualAngle :: Float
        )
    -- Update face colors and add them to screen
    render window renderer
    mapM_ (addFaceToScene cube) faceObjs
  where FaceAnimation {..} = animation

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
          "angleX" @: Float
          "angleY" @: Float
          "rotatingFace" @: Float
          "faceAngle" @: Float
          "time" @: Float

  storage <- LambdaCubeGL.allocStorage inputSchema

  -- Create a standard Rubik's cube model encapsulated in a data structure
  let cube  = makeCube 3
  -- Generate meshes for each face of the cube, and pair them with the its
  -- direction
  let faces = [ (faceMeshes d 3 3, d) | d <- directions ]

  -- Upload each generated face to the GPU memory and extract the graphics
  -- object from the IO Monad
  -- faceObjs :: [FaceObject]
  faceObjs <- forM faces $ \(face, faceDir) -> do
    obj <- uploadFaceToGPU storage face
    return $ FaceObject obj faceDir

  time <- getTimeF
  let animation = FaceAnimation U Clockwise 0.0 0.0 0.0 (-pi / 2) 0.65
  let perms      = words "U R F L B D" -- "L' R' B U D' B'"
  let circlePerm = ["U", "R", "U'", "L'", "U", "R'", "U'", "L"]
  -- Allocate GL pipeline
  renderer <- LambdaCubeGL.allocRenderer pipelineDesc
  LambdaCubeGL.setStorage renderer storage >>= \case -- check schema compatibility
    Just err -> putStrLn err
    Nothing  -> do
      let env   = Env win renderer storage faceObjs
      let world = World cube 0.0 0.0 perms animation time
      runGraphicContext (renderLoop' world) env -- Entry point of the Rendering Loop
  LambdaCubeGL.disposeRenderer renderer
  GLFW.destroyWindow win
  GLFW.terminate
