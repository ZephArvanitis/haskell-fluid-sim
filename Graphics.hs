module Graphics (
  enable, disable, Option(..),
  text,
  quads, tris, wires, wirelines,
  point, vertex,
  rgb, rgba,
  localize, rotate, translate, scale,
  normal, texcoord,
  PointLight(..), PointAtInfinity(..), Specular(..), Diffuse(..),
  initializeGraphics,
  temporarily, temporarilyIf,
  Capability(..),
  useTexture
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
getStateVar (Light i)  = light $ GL.Light $ int i
getStateVar Texture = texture Texture2D
getStateVar Blend = blend

enable :: MonadIO m => Option -> m ()
enable opt = liftIO $ getStateVar opt $= Enabled

disable :: MonadIO m => Option -> m ()
disable opt = liftIO $ getStateVar opt $= Disabled

int :: Int -> GLint
int = fromIntegral

real :: Float -> GLfloat
real = realToFrac

text :: (MonadIO m, Font f) => Int -> Int -> f -> String -> m ()
text x y font string = liftIO $ do
  rasterPos $ Vertex2 (int x) (int y)
  renderString font string
  rasterPos $ Vertex2 0 (int 0)

quads :: MonadIO m => IO () -> m ()
quads = liftIO . renderPrimitive Quads

wirelines :: MonadIO m => IO () -> m ()
wirelines = liftIO . renderPrimitive Lines

tris :: MonadIO m => IO () -> m ()
tris = liftIO . renderPrimitive Triangles

wires :: MonadIO m => IO () -> m ()
wires = liftIO . renderPrimitive LineLoop

point :: Int -> Int -> IO ()
point x y = GL.vertex $ Vertex2 (int x) (int y)

vertex :: Float -> Float -> Float -> IO ()
vertex x y z = liftIO $ GL.vertex $ Vertex3 (real x) (real y) (real z)

rgb :: MonadIO m => Float -> Float -> Float -> m ()
rgb r g b = liftIO $ color $ Color3 (real r) (real g) (real b)

rgba :: MonadIO m => Float -> Float -> Float -> Float -> m ()
rgba r g b a = liftIO $ color $ Color4 (real r) (real g) (real b) (real a)

localize :: (MonadIO m) => IO a -> m a
localize = liftIO . preservingMatrix

rotate :: Float -> Float -> Float -> Float -> IO ()
rotate angle x y z = liftIO $ GL.rotate (real angle) $ Vector3 (real x) (real y) (real z)

translate :: Float -> Float -> Float -> IO ()
translate x y z = liftIO $ GL.translate $ Vector3 (real x) (real y) (real z)

scale :: Float -> Float -> Float -> IO ()
scale x y z = liftIO $ GL.scale (real x) (real y) (real z)

normal :: Float -> Float -> Float -> IO ()
normal x y z = liftIO $ GL.normal $ Normal3 (real x) (real y) (real z)

texcoord :: Float -> Float -> IO ()
texcoord u v = texCoord $ TexCoord2 (real u) (real v)

temporarilyIf :: MonadIO m => Bool -> Capability -> Option -> m b -> m b
temporarilyIf cond status opt action =
  if cond
  then temporarily status opt action
  else action

temporarily :: MonadIO m => Capability -> Option -> m b -> m b
temporarily status opt action = do
    let statevar = getStateVar opt
    oldStatus <- liftIO $ get statevar
    liftIO $ statevar $= status
    val <- action
    liftIO $ statevar $= oldStatus
    return val

useTexture :: MonadIO m => TextureObject -> m a -> m a
useTexture tex action = do
    liftIO (textureBinding Texture2D $= Just tex)
    val <- action
    liftIO (textureBinding Texture2D $= Nothing)
    return val

initializeGraphics :: Int -> Int -> [PointLight] -> IO () 
initializeGraphics width height lights = do
  -- Initialize the viewport and perspective.
  viewport $= (Position 0 0, Size (int width) (int height))

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
    position (GL.Light i) $= Vertex4 (real x) (real y) (real z) 0.0
    specular (GL.Light i) $= Color4 (real sr) (real sg) (real sb) 1.0
    diffuse  (GL.Light i) $= Color4 (real dr) (real dg) (real db) 1.0
