{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Simulator (
  initialize,
  terminate,
  isRunning,
  isPaused,
  update,
  draw,
  addHelpText,
  addMesh,
  deleteMesh,
  SimulatorData(..),
  waitForNextFrame,
  registerHeld,
  registerPressed,
  Simulator,
  runSimulator
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Array((!))
import qualified Data.Array as Array
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLUT.Objects (renderObject, Flavour(Solid), Object(Cone))
import Graphics.UI.GLUT.Fonts as Fonts
import Graphics.UI.GLUT.Begin (mainLoopEvent)
import qualified Graphics.UI.GLUT.Initialization as GLUTInit
import qualified Graphics.UI.GLUT.Window as Window
import qualified Graphics.UI.GLUT.Callbacks.Window as Callbacks
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM(atomically)
import Control.Concurrent.Chan
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted(msDelay)
import Data.Int(Int64)
import Control.Monad
import Control.Applicative ((<$>))
import Control.Monad.State
import Codec.Picture
import Data.Vector.Storable (unsafeWith)
import System.Exit

import ObjectParser
import Graphics

import GHC.Float
import Data.Map(Map, insert, delete, elems, empty)
import qualified Data.Map as Map
import Control.Lens

data KeyButtonState = Pressed | Released deriving Show
type KeyCallback = (KeyButtonState -> SimulatorUpdate)
type Simulator a = StateT SimulatorData IO a 
type SimulatorUpdate = Simulator ()

data SimulatorData = SimulatorData {
	-- Main loop will exit if running becomes false.
	_running :: Bool,

	-- Pause simulation but not visualization
	_paused :: Bool,

  -- Ticker for frame rate
  _frameTicker :: Chan (),

	-- Use help overlay
	_showingHelp :: Bool,

	-- Display coordinate axes
	_showingAxes :: Bool,

	-- Enable wireframe drawing
	_wireframeEnabled :: Bool,

	-- Whether lighting is enabled
	_lightingEnabled :: Bool,

	-- Vertices and faces to draw
	_meshes :: [Mesh],

	-- Whether to use texturing
	_texturesEnabled :: Bool,

	-- Initial translation
	_phi      :: Float,
	_theta    :: Float,
	_distance :: Float,

  -- Keyboard help things
  _keyboardHelpStrings :: [(String, String)],

  -- Callback stuff
  _keyCallbacks :: Map Char KeyCallback,

  -- Things that happen every frame
  _frameActions :: Map Char SimulatorUpdate,

  -- Key event channel
  _keyEventChannel :: TChan (Char, KeyButtonState),

  -- Textures!
  _environmentTextures :: (GL.TextureObject, GL.TextureObject)
  }
makeLenses ''SimulatorData

type Vector = ObjectParser.Vertex

-- Global variables
fps :: Int64
fps = 30

millisPerFrame :: Int64
millisPerFrame = 1000 `div` fps

-- Motion rates
moveRate :: Float
moveRate = 0.1

rotateRate :: Float
rotateRate = 2.0

-- Camera positioning parameters
initPhi :: Float
initPhi = 20.0

initTheta :: Float
initTheta = 40.0

initDistance :: Float
initDistance = 6.0

minDistance :: Float
minDistance = 1.0

maxDistance :: Float
maxDistance = 10

minTheta :: Float
minTheta = 0

maxTheta :: Float
maxTheta = 89.5

roomSize :: Float
roomSize = maxDistance * 1.1

loadTexture :: String -> IO ()
loadTexture filename = do
  eitherImage <- readImage filename
  case eitherImage of 
    (Left err) -> print err >> exitWith (ExitFailure 1)
    (Right image) -> case image of 
      (ImageRGB8 (Image width height dat)) ->
        -- Access the data vector pointer
        unsafeWith dat $ \ptr -> 
          -- Generate the texture
          GL.texImage2D
            -- No cube map
            Nothing
            -- No proxy
            GL.NoProxy
            -- No mipmaps
            0
            -- Internal storage format: use R8G8B8A8 as internal storage
            GL.RGBA8
            -- Size of the image
            (GL.TextureSize2D (fromIntegral width) (fromIntegral height))
            -- No borders
            0
            -- The pixel data: the vector contains Bytes, in RGBA order
            (GL.PixelData GL.RGB GL.UnsignedByte ptr)
      _ -> error "Unknown image type"

initSimulatorState :: IO SimulatorData
initSimulatorState = do
  ticker <- newChan
  keyChannel <- newTChanIO

  let loadTextureObject filename = do
        [texName] <- GL.genObjectNames 1
        GL.textureBinding GL.Texture2D $= Just texName
        loadTexture filename
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
        return texName

  wallTex <- loadTextureObject "data/glass.png"
  groundTex <- loadTextureObject "data/table.png"
  return SimulatorData {
    _running = True,
    _paused = False,
    _frameTicker = ticker,
    _showingHelp = False,
    _showingAxes = False,
    _wireframeEnabled = False,
    _lightingEnabled = True,
    _meshes = [],
    _texturesEnabled = True,
    _phi      = initPhi,
    _theta    = initTheta,
    _distance = initDistance,
    _keyboardHelpStrings = [
      ("ESC or Q", "Exit the simulation."),
      ("W/S",      "Move closer or farther."),
      ("A/D",      "Rotate around the simulation."),
      ("R/F",      "Increase/decrease zenith angle."),
      ("L",        "Toggle lighting."),
      ("T",        "Toggle textures."),
      ("V",        "Toggle wireframe models."),
      ("H",        "Toggle the help menu."),
      ("X",        "Toggle global coordinate axes."),
      ("Space",    "Reset to original camera position.")
      ],
    _keyCallbacks = empty,
    _frameActions = empty,
    _keyEventChannel = keyChannel,
    _environmentTextures = (wallTex, groundTex)
  }

width :: Integral a => a
width = 640

height :: Integral a => a
height = 480

aspectRatio :: Fractional a => a
aspectRatio = fromIntegral (width :: Integer) / fromIntegral (height :: Integer)

initialize :: IO SimulatorData
initialize = do
  void $ GLUTInit.initialize "Simulator" []
  GLUTInit.initialDisplayMode $= [GLUTInit.RGBMode, GLUTInit.WithDepthBuffer]
  void $ Window.createWindow "Simulator"
  Window.windowSize $= GL.Size width height
  Window.windowTitle $= "Fluid Simulation Thingamadoop" 
  Window.windowStatus $= Window.Shown

  state <- initSimulatorState
  let channel = state^.keyEventChannel :: TChan (Char, KeyButtonState)
      keyCallback direc char _ = do
        print (direc, char)
        atomically $ writeTChan channel (char, direc)
      pressedCallback = keyCallback Pressed
      releasedCallback = keyCallback Released
  Callbacks.keyboardCallback $= Just pressedCallback
  Callbacks.keyboardUpCallback $= Just releasedCallback

  void $ repeatedTimer (writeChan (state^.frameTicker) ()) $ msDelay millisPerFrame
  initGL
  putStrLn "Running simulator!"
  snd <$> runStateT initKeys state

runSimulator :: Simulator a -> IO () 
runSimulator action = initialize >>= runStateT action >> terminate

toggle :: Lens' SimulatorData Bool -> Simulator ()
toggle booleanLens = do
  bool <- use booleanLens
  booleanLens .= not bool

bound :: Lens' SimulatorData Float -> Float -> Float -> Lens' SimulatorData Float
bound numLens min max = lens getter setter
  where getter object = object^.numLens
        setter object newValue = numLens .~ value $ object
          where value 
                  | newValue < min = min
                  | newValue > max = max
                  | otherwise = newValue

initKeys :: SimulatorUpdate
initKeys = do
    forM_ triggers registerTrigger
    forM_ holds registerHeldFunc
  where
    registerTrigger (char, updater) = registerPressed char updater
    triggers =
      [ ('q', running .= False )
      , ('h', toggle showingHelp)
      , ('l', toggle lightingEnabled)
      , ('x', toggle showingAxes)
      , ('v', toggle wireframeEnabled)
      , ('t', toggle texturesEnabled)
      , (' ', do phi      .= initPhi
                 theta    .= initTheta
                 distance .= initDistance)
      ]

    registerHeldFunc (char, updater) = registerHeld char updater
    holds = 
      [ ('f', bound theta minTheta maxTheta += rotateRate)
      , ('r', bound theta minTheta maxTheta -= rotateRate)
      , ('d', phi += rotateRate)
      , ('a', phi -= rotateRate)
      , ('s', bound distance minDistance maxDistance += moveRate)
      , ('w', bound distance minDistance maxDistance -= moveRate)
      ]

initGL :: IO () 
initGL = do
    initializeGraphics width height lights
    enable Lighting
    mapM_ (enable . Light) [0, 1, 2]
    enable Texture

  where lights =
          [ PointLight (PointAtInfinity   1.0    1.0  1.0) specular diffuse
          , PointLight (PointAtInfinity (-1.0) (-1.0) 1.0) specular diffuse
          , PointLight (PointAtInfinity   1.0  (-1.0) 1.0) specular diffuse
          ]
        specular = Specular 0.1 0.1 0.1
        diffuse = Diffuse 1.0 1.0 1.0

waitForNextFrame :: SimulatorUpdate
waitForNextFrame = use frameTicker >>= liftIO . readChan

update :: SimulatorUpdate
update = do
  input <- collectInput
  actions <- elems <$> use frameActions
  sequence_ actions
  forM_ input updateWithKeyPress
  where
    updateWithKeyPress (key, buttonState) = do
      callbacks <- use keyCallbacks
      case Map.lookup key callbacks of
        Nothing -> return ()
        Just callback -> callback buttonState 

collectInput :: Simulator [(Char, KeyButtonState)]
collectInput = do
  chan <- use keyEventChannel
  empty <- liftIO $ atomically $ isEmptyTChan chan
  if empty then return [] else do
    thing <- liftIO $ atomically $ readTChan chan
    things <- collectInput
    return $ thing : things

draw :: SimulatorUpdate
draw = do
  liftIO mainLoopEvent
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  -- Perspective or something
  liftIO $ GL.matrixMode $= GL.Projection
  liftIO GL.loadIdentity
  liftIO $ GLU.perspective 45.0 aspectRatio 0.1 100.0

  liftIO $ GL.matrixMode $= GL.Modelview 0
  liftIO GL.loadIdentity
  drawScene

  liftIO $ GL.matrixMode $= GL.Projection
  liftIO GL.loadIdentity
  liftIO $ GL.ortho 0.0  (fromIntegral (width :: Integer))  0 (fromIntegral (height :: Integer))  0.0 30.0

  liftIO $ GL.matrixMode $= GL.Modelview 0
  liftIO GL.loadIdentity
  drawOverlays

  liftIO GL.flush
  liftIO Window.swapBuffers

radian ::  Floating a => a -> a
radian degrees = pi * (degrees / 180.0)

cross :: Vector -> Vector -> Vector
cross (ObjectParser.Vertex x1 y1 z1) (ObjectParser.Vertex x2 y2 z2) = ObjectParser.Vertex {
  x = y1*z2 - z1*y2,
  y = z1*x2 - x1*z2,
  z = x1*y2 - y1*x2
}

drawScene :: SimulatorUpdate
drawScene = do
  -- Compute camera location based on spherical coordinates
  distance <- use distance
  theta <- radian <$> use theta
  phi <- radian <$> use phi
  meshes <- use meshes
  let cameraX = float2Double $ distance * sin theta * cos phi
      cameraY = float2Double $ distance * sin theta * sin phi
      cameraZ = float2Double $ distance * cos theta
      camera = Vertex (double2Float cameraX) (double2Float cameraY) (double2Float cameraZ)

  globalAxes <- use showingAxes
  lighting <- use lightingEnabled
  wireframing <- use wireframeEnabled

  -- Compute "up" vector which defines camera orientation. We do this 
  -- by taking the cross product of the camera vector with the unit vector
  -- pointing in the direction of maximal increase of zenith.
  let azimuthGrad = Vertex (-sin phi) (cos phi) 0
      up = cross camera azimuthGrad

  -- Set up camera direction
  liftIO $ GLU.lookAt (GL.Vertex3 (realToFrac cameraX) (realToFrac cameraY) (realToFrac cameraZ :: GL.GLdouble)) (GL.Vertex3 0 0 0.5) (GL.Vector3 (realToFrac $ x up) (realToFrac $ y up) (realToFrac $ z up))

  -- Disable lighting when using wireframing
  temporarilyIf (lighting && wireframing) Disabled Lighting $ do

    -- Draw simulation boundaries to avoid ugly black background
    drawGround

    -- Draw all meshes
    forM_ meshes drawMesh

    when globalAxes drawCoordinateAxes

drawOverlays :: SimulatorUpdate
drawOverlays = 
  let heading = "Keyboard Commands"
      overlayBorder = 30
      borderPadding = 50
      lineSpacing = 30
      tabLocation = 300 
      font = Fonts.Helvetica18
      bold = Fonts.TimesRoman24
      textOffset = overlayBorder + borderPadding in do

    showOverlay <- use showingHelp
    when showOverlay $ temporarily Disabled Lighting $ do

      -- Allow blending for semi-transparent overview
      enable Blend
      liftIO $ GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

      -- Draw transparent quad covering most of scene
      rgba 0.1 0.1 0.1 0.8
      quads $ do
        point overlayBorder overlayBorder
        point (width-overlayBorder) overlayBorder
        point (width-overlayBorder) (height-overlayBorder)
        point overlayBorder (height-overlayBorder)

      disable Blend

      -- Heading color
      rgb 0.8 0.8 0.8

      -- Help text heading
      headingWidth <- liftIO $ fromIntegral <$> Fonts.stringWidth bold heading
      text ((width-headingWidth) `div` 2) (height - overlayBorder - 30) bold heading

      -- Overlay help text and color
      rgb 0.5 0.5 0.5

      helpStrsWithIndices <- use $ keyboardHelpStrings.to (zip [1..])
      forM_ helpStrsWithIndices $ \(i, (key, helpText)) -> do
        let yLoc = textOffset + lineSpacing * i
            xLoc = textOffset
        text xLoc yLoc font key
        text tabLocation yLoc font helpText

drawGround :: SimulatorUpdate
drawGround = do
  (wallTex, groundTex) <- use environmentTextures

  -- Draw ground quad
  useTexture groundTex $ quads $ do
    normal 0 0 1
    quad [(-roomSize, -roomSize, 0),
          (-roomSize, roomSize, 0),
          (roomSize, roomSize, 0),
          (roomSize, -roomSize, 0)]

  -- Draw walls
  useTexture wallTex $ quads $ do
    normal 1 0 0
    quad [(-roomSize, -roomSize, 0),
          (-roomSize, roomSize, 0),
          (-roomSize, roomSize, roomSize),
          (-roomSize, -roomSize, roomSize)]

    normal (-1) 0 0
    quad [(roomSize, -roomSize, 0),
          (roomSize, roomSize, 0),
          (roomSize, roomSize, roomSize),
          (roomSize, -roomSize, roomSize)]

    normal 0 1 0
    quad [(-roomSize, -roomSize, 0),
          (roomSize,    -roomSize, 0),
          (roomSize,    -roomSize, roomSize),
          (-roomSize, -roomSize, roomSize)]

    normal 0 (-1) 0
    quad [(-roomSize, roomSize, 0),
          (roomSize, roomSize, 0),
          (roomSize, roomSize, roomSize),
          (-roomSize, roomSize, roomSize)]

quad ::  [(Float, Float, Float)] -> IO ()
quad [(x1, y1, z1),
      (x2, y2, z2),
      (x3, y3, z3),
      (x4, y4, z4)] = do
  texcoord 0 0
  vertex x1 y1 z1
  texcoord 0 1
  vertex x2 y2 z2
  texcoord 1 1
  vertex x3 y3 z3
  texcoord 1 0
  vertex x4 y4 z4
quad _ = undefined

-- | Draw coordinate axes and arrows in colors, without lighting.
drawCoordinateAxes :: SimulatorUpdate
drawCoordinateAxes = temporarily Disabled Lighting $ do
  -- Red x-axis line.
  rgb 1.0 0.0 0.0
  wirelines $ do
    vertex 0.0 0.0 0.0
    vertex 1.0 0.0 0.0

  -- red arrow.
  localize $ do  
    translate 1.0 0.0 0.0
    rotate 90 0.0 1.0 0.0
    renderObject Solid $ Cone 0.04 0.2 10 10

  -- green y-axis line.
  rgb 0.0 1.0 0.0
  wirelines $ do
    vertex 0.0 0.0 0.0
    vertex 0.0 1.0 0.0

  -- green arrow.
  localize $ do
    translate 0.0 1.0 0.0
    rotate (-90) 1.0 0.0 0.0
    renderObject Solid $ Cone 0.04 0.2 10 10

  -- blue z-axis line.
  rgb 0.0 0.0 1.0
  wirelines $ do
    vertex 0.0 0.0 0.0
    vertex 0.0 0.0 1.0

  -- Blue arrow.
  localize $ do
    translate 0.0 0.0 1.0
    rotate (-90) 0.0 0.0 1.0
    renderObject Solid $ Cone 0.04 0.2 10 10

drawMesh :: Mesh -> SimulatorUpdate
drawMesh mesh = do
  let verts = vertices mesh
      fs = faces mesh
      norms = vertexNormals mesh
  wireframe <- use wireframeEnabled

	-- Apply local transformations
  localize $ do
    translate (dx mesh) (dy mesh) (dz mesh) 
    rotate (rz mesh) 0 0 1 
    rotate (ry mesh) 0 1 0 
    rotate (rx mesh) 1 0 0 
    scale (sx mesh) (sy mesh) (sz mesh) 

    -- Draw each face separately.
    forM_ (Array.elems fs) $ \face -> do
      when wireframe $ rgb 1.0 1.0 1.0
      let mode = if wireframe then wires else
            case face of
              Triangle {} -> tris
              Quad {} -> quads

      let getVertIndices (Triangle a b c) = [a, b, c]
          getVertIndices (Quad a b c d) = [a, b, c, d]
          faceData vals = map (vals !) $ getVertIndices face
      mode $
        forM_ (zip (faceData verts) (faceData norms)) $ \(vert, norm) -> do
          normal (x norm) (y norm) (z norm)
          vertex (x vert) (y vert) (z vert)


terminate :: IO ()
terminate = do
  window <- GL.get Window.currentWindow
  case window of
    Nothing -> return ()
    Just win -> 
      void $ Window.destroyWindow win

-- Key registering

-- Raw
registerKey :: Char -> KeyCallback -> SimulatorUpdate
registerKey char callback = keyCallbacks %= insert char callback

-- Nice
registerHeld :: Char -> SimulatorUpdate -> SimulatorUpdate
registerHeld char updater = registerKey char callback
  where 
    callback buttonState = frameActions %= newFrameActions
      where
        newFrameActions =
          case buttonState of
            Released -> delete char
            Pressed -> insert char updater

registerPressed :: Char -> SimulatorUpdate -> SimulatorUpdate
registerPressed char updater =
  registerKey char $ \buttonState ->
    case buttonState of
      Pressed -> updater
      Released -> return ()

-- Things that don't require IO
isRunning :: Simulator Bool
isRunning = use running

isPaused :: Simulator Bool
isPaused = use paused

addHelpText ::  String -> String -> SimulatorUpdate
addHelpText key text = keyboardHelpStrings %= ((key, text):)

addMesh :: Mesh -> SimulatorUpdate
addMesh mesh = meshes %= (mesh:)

deleteMesh :: MeshName -> SimulatorUpdate
deleteMesh meshname = meshes %= filter ((/= meshname) . name) 
