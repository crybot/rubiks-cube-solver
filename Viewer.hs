{-# LANGUAGE RecordWildCards, PackageImports, LambdaCase, OverloadedStrings #-}
import Data.Map (Map)
import "GLFW-b"  Graphics.UI.GLFW              as GLFW
import qualified Data.Map                      as Map
import qualified Data.Vector                   as V

import           LambdaCube.GL                 as LambdaCubeGL -- renderer
import           LambdaCube.GL.Mesh            as LambdaCubeGL

import           Codec.Picture                 as Juicy

import           Data.Aeson
import qualified Data.ByteString               as SB

import System.Environment
import Data.Text (unpack,Text)
import Data.List (groupBy,nub)
import Data.Maybe
import Control.Monad

import Codec.Wavefront
import MtlParser


objToMesh :: WavefrontOBJ -> [(Mesh, Maybe Text)]
objToMesh WavefrontOBJ {..} =
  [ (toMesh faceGroup, elMtl . head $ faceGroup) | faceGroup <- faces ] where
  faces = groupBy (\a b -> elMtl a == elMtl b) (V.toList objFaces)
  toMesh l = Mesh
    { mAttributes = Map.fromList
                      [ ("position", A_V4F position)
                      , ("normal"  , A_V3F normal)
                      , ("uvw"     , A_V3F texcoord)
                      ]
    , mPrimitive  = P_Triangles
    }   where
    triangulate (Triangle a b c) = [a, b, c]
    triangulate (Quad a b c d  ) = [a, b, c, c, d, a]
    triangulate (Face a b c l) =
      a : b : c : concatMap (\(x, y) -> [a, x, y]) (zip (c : l) l) -- should work for convex polygons without holes
    defaultPosition = Location 0 0 0 0
    defaultNormal   = Normal 0 0 0
    defaultTexCoord = TexCoord 0 0 0
    v !- i = v V.!? (i - 1)
    toVertex FaceIndex {..} =
      ( let Location x y z w =
              fromMaybe defaultPosition (objLocations !- faceLocIndex)
        in  V4 x y z w
      , let Normal x y z =
              fromMaybe defaultNormal ((objNormals !-) =<< faceNorIndex)
        in  V3 x y z
      , let TexCoord x y z = fromMaybe
              defaultTexCoord
              ((objTexCoords !-) =<< faceTexCoordIndex)
        in  V3 x y z
      )
    (position, normal, texcoord) =
      V.unzip3
        . V.concat
        . map (V.fromList . map toVertex . triangulate . elValue)
        $ l


loadOBJ :: String -> IO (Either String ([(Mesh, Maybe Text)], MtlLib))
loadOBJ fname = fromFile fname >>= \case -- load geometry
  Left  err                   -> putStrLn err >> return (Left err)
  Right obj@WavefrontOBJ {..} -> do
    -- load materials
    mtlLib <- mconcat . V.toList <$> mapM (readMtl . unpack) objMtlLibs
    return $ Right (objToMesh obj, mtlLib)

loadOBJToGPU :: String -> IO (Either String ([(GPUMesh, Maybe Text)], MtlLib))
loadOBJToGPU fname = loadOBJ fname >>= \case
  Left  err                 -> return $ Left err
  Right (subModels, mtlLib) -> do
    gpuSubModels <- forM subModels $ \(mesh, mat) ->
      LambdaCubeGL.uploadMeshToGPU mesh >>= \a -> return (a, mat)
    return $ Right (gpuSubModels, mtlLib)

uploadMtlLib :: MtlLib -> IO (Map Text (ObjMaterial,TextureData))
uploadMtlLib mtlLib = do
  -- collect used textures
  let usedTextures = nub . concatMap (maybeToList . mtl_map_Kd) $ Map.elems mtlLib
      whiteImage = Juicy.ImageRGB8 $ Juicy.generateImage (\_ _ -> Juicy.PixelRGB8 255 255 255) 1 1
      checkerImage = Juicy.ImageRGB8 $ Juicy.generateImage (\x y -> if mod (x + y) 2 == 0 then Juicy.PixelRGB8 0 0 0 else Juicy.PixelRGB8 255 255 0) 2 2
  checkerTex <- LambdaCubeGL.uploadTexture2DToGPU checkerImage
  -- load images and upload to gpu
  textureLib <- forM (Map.fromList $ zip usedTextures usedTextures) $ \fname -> Juicy.readImage fname >>= \case
    Left err  -> putStrLn err >> return checkerTex
    Right img -> LambdaCubeGL.uploadTexture2DToGPU img
  whiteTex <- LambdaCubeGL.uploadTexture2DToGPU whiteImage
  -- pair textures and materials
  return $ (\a -> (a, maybe whiteTex (fromMaybe checkerTex . flip Map.lookup textureLib) . mtl_map_Kd $ a)) <$> mtlLib

addOBJToObjectArray :: GLStorage -> String -> [(GPUMesh, Maybe Text)] -> Map Text (ObjMaterial,TextureData) -> IO [LambdaCubeGL.Object]
addOBJToObjectArray storage slotName objMesh mtlLib = forM objMesh $ \(mesh,mat) -> do
  obj <- LambdaCubeGL.addMeshToObjectArray storage slotName ["diffuseTexture","diffuseColor"] mesh -- diffuseTexture and diffuseColor values can change on each model
  case mat >>= flip Map.lookup mtlLib of
    Nothing -> return ()
    Just (ObjMaterial{..},t) -> LambdaCubeGL.updateObjectUniforms obj $ do
      "diffuseTexture" @= return t -- set model's diffuse texture
      "diffuseColor" @= let (r,g,b) = mtl_Kd in return (V4 r g b mtl_Tr)
  return obj


----------------------------------------------------
--  See:  http://lambdacube3d.com/getting-started
----------------------------------------------------

main :: IO ()
main = do
  Just pipelineDesc <- decodeStrict <$> SB.readFile "viewer.json"

  win <- initWindow "Rubik's Cube Viewer with LambdaCube3D" 640 640
  -- setup render data
  let inputSchema = makeSchema $ do
        -- object models
        defObjectArray "objects" Triangles $ do
          "position" @: Attribute_V4F
          "normal" @: Attribute_V3F
          "uvw" @: Attribute_V3F
        -- defObjectArray "objects" Triangles $ do
          -- "position"  @: Attribute_V3F
          -- "uv"        @: Attribute_V3F
        defUniforms $ do
          "time" @: Float
          "diffuseColor" @:V4F
          "diffuseTexture" @: FTexture2D

  storage <- LambdaCubeGL.allocStorage inputSchema

  -- load OBJ geometry and material descriptions
  Right (objMesh, mtlLib) <- loadOBJToGPU "cube.obj"
  -- load materials textures
  gpuMtlLib <- uploadMtlLib mtlLib
  -- add OBJ to pipeline input
  addOBJToObjectArray storage "objects" objMesh gpuMtlLib

  -- upload geometry to GPU and add to pipeline input
  -- LambdaCubeGL.uploadMeshToGPU triangleA
    -- >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []
  -- LambdaCubeGL.uploadMeshToGPU triangleB >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []

  {-
    -- load image and upload texture
    Right img <- Juicy.readImage "logo.png"
    textureData <- LambdaCubeGL.uploadTexture2DToGPU img
    -}

  -- allocate GL pipeline
  renderer <- LambdaCubeGL.allocRenderer pipelineDesc
  LambdaCubeGL.setStorage renderer storage >>= \case -- check schema compatibility
    Just err -> putStrLn err
    Nothing  -> loop
     where
      loop = do
        -- update graphics input
        GLFW.getWindowSize win >>= \(w, h) ->
          LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
        LambdaCubeGL.updateUniforms storage $ do
          -- "diffuseTexture" @= return textureData
          "time" @= do
            Just t <- GLFW.getTime
            return (realToFrac t :: Float)
        -- render
        LambdaCubeGL.renderFrame renderer
        GLFW.swapBuffers win
        GLFW.pollEvents

        let keyIsPressed k = fmap (== KeyState'Pressed) $ GLFW.getKey win k
        escape <- keyIsPressed Key'Escape
        if escape then return () else loop

  LambdaCubeGL.disposeRenderer renderer
  -- LambdaCubeGL.disposeStorage storage
  GLFW.destroyWindow win
  GLFW.terminate

-- geometry data: triangles
triangleA :: LambdaCubeGL.Mesh
triangleA = Mesh
  { mAttributes =
    Map.fromList
      [ ( "position"
        , A_V3F $ V.fromList
          [V3 0.2 0.2 0.2, V3 0.2 (-0.2) (0.2), V3 (-0.2) (-0.2) (-0.2)]
        )
      , ("uv", A_V3F $ V.fromList [V3 1 1 1, V3 0 1 1, V3 0 0 1])
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
