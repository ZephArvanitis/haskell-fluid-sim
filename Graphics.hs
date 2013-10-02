module Graphics (
  enable, disable, Option(..),
  text,
  quads, tris, wires,
  point, vertex,
  rgb, rgba,
  rotate, translate, scale,
  normal, texcoord,
  PointLight(..), PointAtInfinity(..), Specular(..), Diffuse(..),
  initializeGraphics
) where

import Graphics.Rendering.OpenGL hiding (Texture, Light, Blend, Specular, Diffuse, vertex, rotate, translate, scale, normal)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT.Fonts as Fonts
import Control.Monad.Trans
import Control.Monad

data Option = Lighting | Texture | Light Int | Blend

data PointLight = PointLight PointAtInfinity Specular Diffuse
data PointAtInfinity = PointAtInfinity Float Float Float
data Specular = Specular Float Float Float
data Diffuse = Diffuse Float Float Float

getStateVar :: Option -> StateVar Capability
getStateVar Lighting = lighting
getStateVar (Light i)  = light $ GL.Light $ fromIntegral i
getStateVar Texture = texture Texture2D
getStateVar Blend = blend

enable :: MonadIO m => Option -> m ()
enable opt = liftIO $ getStateVar opt $= Enabled

disable :: MonadIO m => Option -> m ()
disable opt = liftIO $ getStateVar opt $= Disabled

text :: (MonadIO m, Font f) => Int -> Int -> f -> String -> m ()
text x y font string = liftIO $ do
  rasterPos $ Vertex2 (fromIntegral x) (fromIntegral y :: GLint)
  renderString font string
  rasterPos $ Vertex2 0 (0 :: GLint)

quads :: MonadIO m => IO () -> m ()
quads = liftIO . renderPrimitive Quads

tris :: MonadIO m => IO () -> m ()
tris = liftIO . renderPrimitive Triangles

wires :: MonadIO m => IO () -> m ()
wires = liftIO . renderPrimitive LineLoop

point :: Int -> Int -> IO ()
point x y = GL.vertex $ Vertex2 (fromIntegral x) (fromIntegral y :: GLint)

vertex :: Float -> Float -> Float -> IO ()
vertex x y z = liftIO $ GL.vertex $ Vertex3 (realToFrac x) (realToFrac y) (realToFrac z :: GLfloat)

rgb :: MonadIO m => Float -> Float -> Float -> m ()
rgb r g b = liftIO $ color $ Color3 (realToFrac r :: GLfloat) (realToFrac g) (realToFrac b)

rgba :: MonadIO m => Float -> Float -> Float -> Float -> m ()
rgba r g b a = liftIO $ color $ Color4 (realToFrac r :: GLfloat) (realToFrac g) (realToFrac b) (realToFrac a)

rotate :: Float -> Float -> Float -> Float -> IO ()
rotate angle x y z = liftIO $ GL.rotate (realToFrac angle) $ Vector3 (realToFrac x) (realToFrac y) (realToFrac z :: GLfloat)

translate :: Float -> Float -> Float -> IO ()
translate x y z = liftIO $ GL.translate $ Vector3 (realToFrac x) (realToFrac y) (realToFrac z :: GLfloat)

scale :: Float -> Float -> Float -> IO ()
scale x y z = liftIO $ GL.scale (realToFrac x) (realToFrac y) (realToFrac z :: GLfloat)

normal :: Float -> Float -> Float -> IO ()
normal x y z = liftIO $ GL.normal $ Normal3 (realToFrac x) (realToFrac y) (realToFrac z :: GLfloat)

texcoord :: Float -> Float -> IO ()
texcoord u v = texCoord $ TexCoord2 (realToFrac u) (realToFrac v :: GLfloat)

initializeGraphics :: Int -> Int -> [PointLight] -> IO () 
initializeGraphics width height lights = do
  -- Initialize the viewport and perspective.
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))

  shadeModel $= Smooth
  clearColor $= Color4 0 0 0 0
  clearDepth $= 1
  depthFunc $= Just Lequal
  hint PerspectiveCorrection $= Nicest

  -- Assign some defaults for material properties.
  let matSpecular = GL.Color4 1.0 1.0 1.0 1.0
      matDiffuse = GL.Color4 0.5 1.0 0.5 0.5
      matShininess = 50.0
  materialSpecular FrontAndBack $= matSpecular
  materialDiffuse FrontAndBack $= matDiffuse
  materialShininess FrontAndBack $= matShininess

  forM_ (zip [1..] lights) $ \(i, PointLight (PointAtInfinity x y z) (Specular sr sg sb) (Diffuse dr dg db)) -> do
    position (GL.Light i) $= Vertex4 (realToFrac x) (realToFrac y) (realToFrac z) (0.0 :: GLfloat)
    specular (GL.Light i) $= Color4 (realToFrac sr) (realToFrac sg) (realToFrac sb) (1.0 :: GLfloat)
    diffuse  (GL.Light i) $= Color4 (realToFrac dr) (realToFrac dg) (realToFrac db) (1.0 :: GLfloat)
