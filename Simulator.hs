module Simulator (
  initialize,
  terminate,
  isRunning,
  isPaused,
  shouldRestart,
  setHasRestarted,
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
import qualified Graphics.UI.GLFW as GLFW
import Data.Array((!), elems)
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLUT.Objects (renderObject, Flavour(Solid), Object(Cone))
import Graphics.UI.GLUT.Fonts as Fonts
import qualified Graphics.UI.GLUT.Initialization as GLUTInit
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM(atomically)
import Control.Concurrent.Chan
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted(msDelay)
import Data.Int(Int64)
import Control.Monad
import Control.Applicative ((<$>))
import Control.Monad.State

import ObjectParser
import Graphics

import Data.List(find)
import GHC.Float

data SimulatorData = SimulatorData {
	-- Main loop will exit if running becomes false.
	running :: Bool,

	-- Pause simulation but not visualization
	paused :: Bool,

  -- Ticker for frame rate
  ticker :: Chan (),

	-- Restart simulation: this is a little funky; the flag to restart is
	-- here, but the simulation restart has to happen in main, so the
	-- function ShouldRestartSimulation() is a little...awkward.
	shouldRestartSimulation :: Bool,

	-- Use help overlay
	helpOverlay :: Bool,

	-- Display coordinate axes
	getGlobalAxes :: Bool,

	-- Enable wireframe drawing
	getWireframe :: Bool,

	-- Whether lighting is enabled
	getLighting :: Bool,

	-- Vertices and faces to draw
	getMeshes :: [Mesh],

	-- Textures to use
	-- Index 0: ground texture
	-- Index 1: wall texture
	textures :: [String],

	-- Whether to use texturing
	texturing :: Bool,

	-- Initial translation
	getPhi      :: Float,
	getTheta    :: Float,
	getDistance :: Float,

  -- Keyboard help things
  keyboardHelpStrings :: [(String, String)],

  -- Callback stuff
  callbacks :: [(Char, KeyCallback)],

  -- Things that happen every frame
  frameActions :: [(Char, SimulatorUpdate)],

  -- Key event channel
  keyChannel :: TChan (GLFW.Key, GLFW.KeyButtonState),

  -- Textures!
  boxTextures :: (GL.TextureObject, GL.TextureObject)
  }

type Simulator a = StateT SimulatorData IO a 

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

initSimulatorState :: IO SimulatorData
initSimulatorState = do
  ticker <- newChan
  keyChannel <- newTChanIO

  let loadTextureObject filename = do
        [texName] <- GL.genObjectNames 1
        GL.textureBinding GL.Texture2D $= Just texName
        GLFW.loadTexture2D filename []
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
        return texName

  wallTex <- loadTextureObject "data/glass.tga"
  groundTex <- loadTextureObject "data/table.tga"
  return SimulatorData {
    running = True,
    paused = False,
    ticker = ticker,
    shouldRestartSimulation = False,
    helpOverlay = False,
    getGlobalAxes = False,
    getWireframe = False,
    getLighting = True,
    getMeshes = [],
    textures = [],
    texturing = True,
    getPhi      = initPhi,
    getTheta    = initTheta,
    getDistance = initDistance,
    keyboardHelpStrings = [
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
    callbacks = [],
    frameActions = [],
    keyChannel = keyChannel,
    boxTextures = (wallTex, groundTex)
  }

width :: Integral a => a
width = 640

height :: Integral a => a
height = 480

aspectRatio :: Fractional a => a
aspectRatio = fromIntegral (width :: Integer) / fromIntegral (height :: Integer)

initialize :: IO SimulatorData
initialize = do
  True <- GLFW.initialize
  GLUTInit.initialize "Simulator" []
  True <- GLFW.openWindow (GL.Size width height) [
    GLFW.DisplayRGBBits 8 8 8,
    GLFW.DisplayDepthBits 16,
    GLFW.DisplayAlphaBits 8
    ] GLFW.Window
  GLFW.windowTitle $= "Fluid Simulation Thingamadoop" 
  state <- initSimulatorState
  repeatedTimer (writeChan (ticker state) ()) $ msDelay millisPerFrame
  GLFW.keyCallback $= curry (atomically . writeTChan (keyChannel state))
  initGL
  snd <$> runStateT initKeys state

runSimulator :: Simulator a -> IO () 
runSimulator action = initialize >>= runStateT action >> terminate

initKeys :: SimulatorUpdate
initKeys = do
    forM_ triggers registerTrigger
    forM_ holds registerHeldFunc
  where
    registerTrigger (char, updater) = registerPressed char updater
    triggers =
      [ ('Q', modify $ \sim -> sim { running = False })
      , ('H', modify $ \sim -> sim { helpOverlay = not $ helpOverlay sim })
      , ('L', modify $ \sim -> sim { getLighting = not $ getLighting sim })
      , ('X', modify $ \sim -> sim { getGlobalAxes = not $ getGlobalAxes sim })
      , ('V', modify $ \sim -> sim { getWireframe = not $ getWireframe sim })
      , ('T', modify $ \sim -> sim { texturing = not $ texturing sim })
      , (' ', modify $ \sim -> sim { getPhi      = initPhi,
                           getTheta    = initTheta,
                           getDistance = initDistance })
      ]

    registerHeldFunc (char, updater) = registerHeld char updater
    bound min max newval
      | newval < min = min
      | newval > max = max
      | otherwise = newval
    holds = 
      [ ('F', modify $ \sim -> sim { getTheta = bound minTheta maxTheta (getTheta sim + rotateRate) })
      , ('R', modify $ \sim -> sim { getTheta = bound minTheta maxTheta (getTheta sim - rotateRate) })
      , ('D', modify $ \sim -> sim { getPhi = getPhi sim + rotateRate })
      , ('A', modify $ \sim -> sim { getPhi = getPhi sim - rotateRate })
      , ('S', modify $ \sim -> sim { getDistance = bound minDistance maxDistance (getDistance sim + moveRate) })
      , ('W', modify $ \sim -> sim { getDistance = bound minDistance maxDistance (getDistance sim - moveRate) })
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
waitForNextFrame = gets ticker >>= liftIO . readChan

update :: SimulatorUpdate
update = do
  input <- collectInput
  actions <- map snd <$> gets frameActions
  sequence_ actions
  forM_ input updateWithKeyPress
  where
    updateWithKeyPress (key, buttonState) = do
      maybeCallback <- gets $ find ((== key) . GLFW.CharKey . fst) . callbacks
      case maybeCallback of
        Nothing -> return ()
        Just (_, callback) -> callback buttonState 

collectInput :: Simulator [(GLFW.Key, GLFW.KeyButtonState)]
collectInput = do
  chan <- gets keyChannel
  empty <- liftIO $ atomically $ isEmptyTChan chan
  if empty then return [] else do
    thing <- liftIO $ atomically $ readTChan chan
    things <- collectInput
    return $ thing : things

type KeyCallback = (GLFW.KeyButtonState -> SimulatorUpdate)
type SimulatorUpdate = Simulator ()

draw :: SimulatorUpdate
draw = do
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
  liftIO GLFW.swapBuffers

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
  distance <- gets getDistance
  theta <- radian <$> gets getTheta
  phi <- radian <$> gets getPhi
  meshes <- gets getMeshes
  let cameraX = float2Double $ distance * sin theta * cos phi
      cameraY = float2Double $ distance * sin theta * sin phi
      cameraZ = float2Double $ distance * cos theta
      camera = Vertex (double2Float cameraX) (double2Float cameraY) (double2Float cameraZ)

  globalAxes <- gets getGlobalAxes
  lighting <- gets getLighting
  wireframing <- gets getWireframe

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

    showOverlay <- gets helpOverlay
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
      helpStrsWithIndices <- gets $ zip [1..] . keyboardHelpStrings
      forM_ helpStrsWithIndices $ \(i, (key, helpText)) -> do
        let yLoc = textOffset + lineSpacing * i
            xLoc = textOffset
        text xLoc yLoc font key
        text tabLocation yLoc font helpText

drawGround :: SimulatorUpdate
drawGround = do
  (wallTex, groundTex) <- gets boxTextures

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
  wireframe <- gets getWireframe

	-- Apply local transformations
  localize $ do
    translate (dx mesh) (dy mesh) (dz mesh) 
    rotate (rz mesh) 0 0 1 
    rotate (ry mesh) 0 1 0 
    rotate (rx mesh) 1 0 0 
    scale (sx mesh) (sy mesh) (sz mesh) 

    -- Draw each face separately.
    forM_ (elems fs) $ \face -> do
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
terminate = GLFW.closeWindow >> GLFW.terminate

-- Key registering

-- Raw
registerKey :: Char -> KeyCallback -> SimulatorUpdate
registerKey char callback = modify $ \simulator -> simulator {
  callbacks = (char, callback) : callbacks simulator
}

-- Nice
registerHeld :: Char -> SimulatorUpdate -> SimulatorUpdate
registerHeld char updater = registerKey char callback
  where 
    callback buttonState = modify $ \sim -> sim { frameActions = newFrameActions $ frameActions sim }
      where
        newFrameActions oldFrameActions =
          case buttonState of
            GLFW.Release -> filter ((/= char) . fst) oldFrameActions
            GLFW.Press -> (char, updater) : oldFrameActions

registerPressed :: Char -> SimulatorUpdate -> SimulatorUpdate
registerPressed char updater =
  registerKey char $ \buttonState ->
    case buttonState of
      GLFW.Press -> updater
      GLFW.Release -> return ()

-- Things that don't require IO
isRunning :: Simulator Bool
isRunning = gets running

isPaused :: Simulator Bool
isPaused = gets paused

shouldRestart :: Simulator Bool
shouldRestart = gets shouldRestartSimulation

setHasRestarted :: SimulatorUpdate
setHasRestarted = modify $ \simulator -> simulator {
    shouldRestartSimulation = False
}

addHelpText ::  String -> String -> SimulatorUpdate
addHelpText key text = modify $ \simulator -> 
  simulator {
    keyboardHelpStrings = keyboardHelpStrings simulator ++ [(key, text)]
  }
    
addMesh :: Mesh -> SimulatorUpdate
addMesh mesh = modify $ \simulator -> simulator {
  getMeshes = mesh : getMeshes simulator
}

deleteMesh :: MeshName -> SimulatorUpdate
deleteMesh meshname = modify $ \simulator -> simulator {
  getMeshes = filter ((/= meshname) . name) $ getMeshes simulator
}
