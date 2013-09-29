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
  registerPressed
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Data.Array((!), elems)
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLUT.Objects (renderObject, Flavour(Solid), Object(Cone))
import Graphics.UI.GLUT.Fonts as Fonts
import Control.Concurrent.Chan
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted(msDelay)
import Data.Int(Int64)
import Control.Monad

import ObjectParser
import Control.Exception(finally)
import Data.List(foldl', find)
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
  frameActions :: [(Char, SimulatorData -> SimulatorData)],

  -- Key event channel
  keyChannel :: Chan (GLFW.Key, GLFW.KeyButtonState),

  -- Textures!
  boxTextures :: (GL.TextureObject, GL.TextureObject)
  }

type Vector = ObjectParser.Vertex
vector ::  Float -> Float -> Float -> Vertex
vector = Vertex

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
initPhi      = 20.0

initTheta :: Float
initTheta    = 40.0

initDistance :: Float
initDistance = 6.0

minDistance :: Float
minDistance  = 1.0

maxDistance :: Float
maxDistance  = 10

roomSize :: GL.GLfloat
roomSize = realToFrac $ maxDistance * 1.1


initSimulatorState :: IO SimulatorData
initSimulatorState = do
  ticker <- newChan
  keyChannel <- newChan

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

initialize :: IO SimulatorData
initialize = do
  True <- GLFW.initialize
  True <- GLFW.openWindow (GL.Size width height) [
    GLFW.DisplayRGBBits 8 8 8,
    GLFW.DisplayDepthBits 1,
    GLFW.DisplayAlphaBits 8
    ] GLFW.Window
  GLFW.windowTitle $= "Fluid Simulation Thingamadoop" 
  GL.depthFunc $= Just GL.Less
  state <- initSimulatorState
  repeatedTimer (writeChan (ticker state) ()) $ msDelay millisPerFrame
  GLFW.keyCallback $= (curry $ writeChan $ keyChannel state)
  initGL
  return state

initGL :: IO () 
initGL = do
  -- Initialize the viewport and perspective.
  GL.viewport $= (GL.Position 0 0, GL.Size width height)

  GL.shadeModel $= GL.Smooth
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.clearDepth $= 1
  GL.depthFunc $= Just GL.Lequal
  GL.hint GL.PerspectiveCorrection $= GL.Nicest

  let specular = GL.Color4 1.0 1.0 1.0 1.0
      diffuse = GL.Color4 0.5 1.0 0.5 0.5
      shininess = 50.0
      light0Position = GL.Vertex4 1.0 1.0 1.0 0.0
      light1Position = GL.Vertex4 (-1.0) (-1.0) 1.0 0.0
      light2Position = GL.Vertex4 1.0 1.0 (-1.0) 0.0
      lightDiffuse = GL.Color4  1.0 1.0 1.0 1.0
      lightSpecular = GL.Color4  0.1 0.1 0.1 1.0

  GL.materialSpecular GL.FrontAndBack $= specular
  GL.materialDiffuse GL.FrontAndBack $= diffuse
  GL.materialShininess GL.FrontAndBack $= shininess

  GL.position (GL.Light 0) $= light0Position
  GL.position (GL.Light 1) $= light1Position
  GL.position (GL.Light 2) $= light2Position

  GL.specular (GL.Light 0) $= lightSpecular
  GL.specular (GL.Light 1) $= lightSpecular
  GL.specular (GL.Light 2) $= lightSpecular

  GL.diffuse (GL.Light 1) $= lightDiffuse
  GL.diffuse (GL.Light 2) $= lightDiffuse

  GL.lighting $= GL.Enabled
  GL.light (GL.Light 0) $= GL.Enabled
  GL.light (GL.Light 1) $= GL.Enabled
  GL.light (GL.Light 2) $= GL.Enabled
  GL.texture GL.Texture2D $= GL.Enabled

waitForNextFrame :: SimulatorData -> IO ()
waitForNextFrame = readChan . ticker

update :: SimulatorData -> IO SimulatorData
update simulator = do
  input <- collectInput simulator
  return $ foldl' updateWithKeyPress simulator input
  where
    updateWithKeyPress simulator (key, buttonState) =
      case find ((== key) . GLFW.CharKey . fst) $ callbacks simulator of
        Nothing -> simulator
        Just (_, callback) -> callback simulator buttonState 

collectInput :: SimulatorData -> IO [(GLFW.Key, GLFW.KeyButtonState)]
collectInput simulator@(SimulatorData{keyChannel = chan}) = do
  empty <- isEmptyChan chan
  if empty then return [] else do
    thing <- readChan chan
    things <- collectInput simulator
    return $ thing : things

type KeyCallback = (SimulatorData -> GLFW.KeyButtonState -> SimulatorData)
type SimulatorUpdate = (SimulatorData -> SimulatorData)

draw :: SimulatorData -> IO ()
draw state = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  -- Perspective or something
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  let aspectRatio = fromIntegral width / fromIntegral height
  GLU.perspective 45.0 aspectRatio 0.1 100.0

  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  drawScene state

  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0.0  (fromIntegral width)  0 (fromIntegral height)  0.0 30.0

  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  drawOverlays state

  GL.flush
  GLFW.swapBuffers

radian degrees = pi * (degrees / 180.0)
cross :: Vector -> Vector -> Vector
cross (ObjectParser.Vertex x1 y1 z1) (ObjectParser.Vertex x2 y2 z2) = ObjectParser.Vertex {
  x = y1*z2 - z1*y2,
  y = z1*x2 - x1*z2,
  z = x1*y2 - y1*x2
}

drawScene :: SimulatorData -> IO ()
drawScene simulator =
  -- Compute camera location based on spherical coordinates
  let distance = getDistance simulator
      theta = radian $ getTheta simulator
      phi = radian $ getPhi simulator
      meshes = getMeshes simulator
      cameraX = float2Double $ distance * sin theta * cos phi
      cameraY = float2Double $ distance * sin theta * sin phi
      cameraZ = float2Double $ distance * cos theta
      camera = vector (double2Float cameraX) (double2Float cameraY) (double2Float cameraZ)

      globalAxes = getGlobalAxes simulator
      lighting = getLighting simulator
      wireframing = getWireframe simulator

      -- Compute "up" vector which defines camera orientation. We do this 
      -- by taking the cross product of the camera vector with the unit vector
      -- pointing in the direction of maximal increase of zenith.
      azimuthGrad = vector (-sin phi) (cos phi) 0
      up = cross camera azimuthGrad in
    do
      -- Set up camera direction
      GLU.lookAt (GL.Vertex3 (realToFrac cameraX) (realToFrac cameraY) (realToFrac cameraZ :: GL.GLdouble)) (GL.Vertex3 0 0 0.5) (GL.Vector3 (realToFrac $ x up) (realToFrac $ y up) (realToFrac $ z up))

      -- Disable lighting when using wireframing
      flip finally (GL.lighting $= GL.Enabled) $ do 
        when (lighting && wireframing) $ GL.lighting $= GL.Disabled

        -- Draw simulation boundaries to avoid ugly black background
        drawGround simulator

        -- Draw all meshes
        forM_ meshes $ drawMesh simulator

        when globalAxes $ drawCoordinateAxes simulator

drawOverlays :: SimulatorData -> IO ()
drawOverlays simulator = 
  let heading = "Keyboard Commands"
      overlayBorder = 30
      borderPadding = 50
      lineSpacing = 30
      tabLocation = 300 
      font = Fonts.Helvetica18
      bold = Fonts.TimesRoman24
      lighting = getLighting simulator
      textOffset = overlayBorder + borderPadding in when (helpOverlay simulator) $ do
    flip finally (GL.lighting $= GL.Enabled >> GL.blend $= GL.Disabled) $ do 
      when (getLighting simulator) $ GL.lighting $= GL.Disabled

      -- Allow blending for semi-transparent overview
      GL.blend $= GL.Enabled

      -- Draw transparent quad covering most of scene
      GL.color (GL.Color4 0.1 0.1 0.1 0.9 :: GL.Color4 GL.GLfloat)
      GL.renderPrimitive GL.Quads $ do
        GL.vertex $ GL.Vertex2 overlayBorder overlayBorder
        GL.vertex $ GL.Vertex2 (width-overlayBorder) overlayBorder
        GL.vertex $ GL.Vertex2 (width-overlayBorder) (height-overlayBorder)
        GL.vertex $ GL.Vertex2 overlayBorder (height-overlayBorder)

      -- Text color
      GL.color (GL.Color3 0.9 0.9 0.9 :: GL.Color3 GL.GLfloat)

      -- Help text heading
      headingWidth <- Fonts.stringWidth bold heading
      GL.preservingMatrix $ do
        GL.rasterPos $ GL.Vertex2 (width-headingWidth `div` 2) (overlayBorder+10)
        Fonts.renderString bold heading

      -- Overlay help text
      let helpStrsWithIndices = zip [1..] $ keyboardHelpStrings simulator 
      forM_ helpStrsWithIndices $ \(i, (key, helpText)) -> do
        let yLoc = textOffset + lineSpacing * i
            xLoc = textOffset
        GL.preservingMatrix $ do
          GL.rasterPos $ GL.Vertex2 xLoc yLoc
          Fonts.renderString font key

          GL.rasterPos $ GL.Vertex2 (tabLocation - xLoc) 0
          Fonts.renderString font helpText

drawGround :: SimulatorData -> IO ()
drawGround simulator = 
  let (wallTex, groundTex) = boxTextures simulator in do
    -- Use a white material to use the texture's natural color.
    GL.materialSpecular GL.FrontAndBack  $= GL.Color4 1.0 1.0 1.0 1.0
    GL.materialDiffuse GL.FrontAndBack   $= GL.Color4 1.0 1.0 0.8 0.5
    GL.materialShininess GL.FrontAndBack $= 100.0

    -- Draw ground quad
    GL.textureBinding  GL.Texture2D $= Just groundTex
    GL.renderPrimitive GL.Quads $ do
      n $ GL.Normal3 0 0 1
      tex $ GL.TexCoord2 0 0
      GL.vertex $ GL.Vertex3 (-roomSize) (-roomSize) 0
      tex $ GL.TexCoord2 0 1
      GL.vertex $ GL.Vertex3 (-roomSize) roomSize 0
      tex $ GL.TexCoord2 1 1
      GL.vertex $ GL.Vertex3 roomSize roomSize 0
      tex $ GL.TexCoord2 1 0
      GL.vertex $ GL.Vertex3 roomSize (-roomSize) 0

    -- Draw walls
    GL.textureBinding GL.Texture2D $= Just wallTex
    GL.renderPrimitive GL.Quads $ do
      n $ GL.Normal3 1 0 0
      tex $ GL.TexCoord2 0 0
      GL.vertex $ GL.Vertex3 (-roomSize) (-roomSize) 0
      tex $ GL.TexCoord2 0 1
      GL.vertex $ GL.Vertex3 (-roomSize) roomSize 0
      tex $ GL.TexCoord2 1 1
      GL.vertex $ GL.Vertex3 (-roomSize) roomSize roomSize
      tex $ GL.TexCoord2 1 0
      GL.vertex $ GL.Vertex3 (-roomSize) (-roomSize) roomSize

      n $ GL.Normal3 (-1) 0 0
      tex $ GL.TexCoord2 0 0
      GL.vertex $ GL.Vertex3 roomSize (-roomSize) 0
      tex $ GL.TexCoord2 0 1
      GL.vertex $ GL.Vertex3 roomSize roomSize 0
      tex $ GL.TexCoord2 1 1
      GL.vertex $ GL.Vertex3 roomSize roomSize roomSize
      tex $ GL.TexCoord2 1 0
      GL.vertex $ GL.Vertex3 roomSize (-roomSize) roomSize

      n $ GL.Normal3 0 1 0
      tex $ GL.TexCoord2 0 0
      GL.vertex $ GL.Vertex3 (-roomSize) (-roomSize) 0
      tex $ GL.TexCoord2 0 1
      GL.vertex $ GL.Vertex3 roomSize (-roomSize) 0
      tex $ GL.TexCoord2 1 1
      GL.vertex $ GL.Vertex3 roomSize (-roomSize) roomSize
      tex $ GL.TexCoord2 1 0
      GL.vertex $ GL.Vertex3 (-roomSize) (-roomSize) roomSize

      n $ GL.Normal3 0 (-1) 0
      tex $ GL.TexCoord2 0 0
      GL.vertex $ GL.Vertex3 (-roomSize) roomSize 0
      tex $ GL.TexCoord2 0 1
      GL.vertex $ GL.Vertex3 roomSize roomSize 0
      tex $ GL.TexCoord2 1 1
      GL.vertex $ GL.Vertex3 roomSize roomSize roomSize
      tex $ GL.TexCoord2 1 0
      GL.vertex $ GL.Vertex3 (-roomSize) roomSize roomSize

    -- Remove texturing
   -- GL.textureBinding GL.Texture2D $= Nothing

v :: GL.Vertex3 GL.GLfloat -> IO ()
v = GL.vertex
c :: GL.Color3 GL.GLfloat -> IO ()
c = GL.color
rot :: GL.GLfloat -> GL.Vector3 GL.GLfloat -> IO ()
rot = GL.rotate
trans :: GL.Vector3 GL.GLfloat -> IO ()
trans = GL.translate
sc :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> IO ()
sc = GL.scale
n :: GL.Normal3 GL.GLfloat -> IO ()
n = GL.normal
tex :: GL.TexCoord2 GL.GLfloat -> IO ()
tex = GL.texCoord

drawCoordinateAxes :: SimulatorData -> IO ()
drawCoordinateAxes simulator = do
  let lighting = getLighting simulator
  -- Draw coordinate axes and arrows in colors, without lighting.
  flip finally (GL.lighting $= GL.Enabled) $ do 
    when (getLighting simulator) $ GL.lighting $= GL.Disabled


    -- Red x-axis line.
    c $ GL.Color3 1.0 0.0 0.0
    GL.renderPrimitive GL.Lines $ do
      v $ GL.Vertex3 0.0 0.0 0.0
      v $ GL.Vertex3 1.0 0.0 0.0

    -- red arrow.
    GL.preservingMatrix $ do  
      trans $ GL.Vector3 1.0 0.0 0.0
      rot 90 $ GL.Vector3 0.0 1.0 0.0
      renderObject Solid $ Cone 0.04 0.2 10 10

    -- green y-axis line.
    c $ GL.Color3 0.0 1.0 0.0
    GL.renderPrimitive GL.Lines $ do
      v $ GL.Vertex3 0.0 0.0 0.0
      v $ GL.Vertex3 0.0 1.0 0.0

    -- green arrow.
    GL.preservingMatrix $ do
      trans $ GL.Vector3 0.0 1.0 0.0
      rot (-90) $ GL.Vector3 1.0 0.0 0.0
      renderObject Solid $ Cone 0.04 0.2 10 10

    -- blue z-axis line.
    c $ GL.Color3 0.0 0.0 1.0
    GL.renderPrimitive GL.Lines $ do
      v $ GL.Vertex3 0.0 0.0 0.0
      v $ GL.Vertex3 0.0 0.0 1.0

    -- Blue arrow.
    GL.preservingMatrix $ do
      trans $ GL.Vector3 0.0 0.0 1.0
      rot (-90) $ GL.Vector3 0.0 0.0 1.0
      renderObject Solid $ Cone 0.04 0.2 10 10

drawMesh :: SimulatorData -> Mesh -> IO ()
drawMesh simulator mesh = do
  let verts = vertices mesh
      fs = faces mesh
      norms = vertexNormals mesh
      wireframe = getWireframe simulator

	-- Apply local transformations
  GL.preservingMatrix $ do
    trans $ GL.Vector3 (realToFrac $ dx mesh) (realToFrac $ dy mesh) (realToFrac $ dz mesh) 
    rot (realToFrac $ rz mesh) $ GL.Vector3 0 0 1 
    rot (realToFrac $ ry mesh) $ GL.Vector3 0 1 0 
    rot (realToFrac $ rx mesh) $ GL.Vector3 1 0 0 
    sc (realToFrac $ sx mesh) (realToFrac $ sy mesh) (realToFrac $ sz mesh) 

    -- Draw each face separately.
    forM_ (elems fs) $ \face -> do
      when wireframe $ c $ GL.Color3 1.0 1.0 1.0
      let mode = if wireframe then GL.LineLoop else
            case face of
              Triangle _ _ _ -> GL.Triangles
              Quad _ _ _ _ -> GL.Quads

      let getVertIndices (Triangle a b c) = [a, b, c]
          getVertIndices (Quad a b c d) = [a, b, c, d]
          faceData vals = map (vals !) $ getVertIndices face
      GL.renderPrimitive mode $
        forM (zip (faceData verts) (faceData norms)) $ \(vertex, normal) -> do
          n $ GL.Normal3 (realToFrac $x normal) (realToFrac $y normal) (realToFrac $z normal)
          v $ GL.Vertex3 (realToFrac $x vertex) (realToFrac $y vertex) (realToFrac $z vertex)


terminate :: IO ()
terminate = GLFW.closeWindow >> GLFW.terminate

-- Key registering

-- Raw
registerKey :: SimulatorData -> Char -> KeyCallback -> SimulatorData
registerKey simulator char callback = simulator {
  callbacks = (char, callback) : callbacks simulator
}

-- Nice
registerHeld :: SimulatorData -> Char -> SimulatorUpdate -> SimulatorData
registerHeld simulator char updater = registerKey simulator char callback
  where 
    callback (simulator@SimulatorData{frameActions = frameActions}) buttonState = 
      simulator { frameActions = newFrameActions }
      where
        newFrameActions =
          case buttonState of
            GLFW.Release -> filter ((/= char) . fst) frameActions
            GLFW.Press -> (char, updater) : frameActions

registerPressed :: SimulatorData -> Char -> SimulatorUpdate -> SimulatorData
registerPressed simulator char updater =
  registerKey simulator char (\simulator _ -> updater simulator)

-- Things that don't require IO
isRunning :: SimulatorData -> Bool
isRunning = running

isPaused :: SimulatorData -> Bool
isPaused = paused

shouldRestart :: SimulatorData -> Bool
shouldRestart = shouldRestartSimulation

setHasRestarted :: SimulatorData -> SimulatorData
setHasRestarted simulator = simulator {
    shouldRestartSimulation = False
}

addHelpText :: SimulatorData -> String -> String -> SimulatorData
addHelpText simulator key text = 
  simulator {
    keyboardHelpStrings = keyboardHelpStrings simulator ++ [(key, text)]
  }
    
addMesh :: SimulatorData -> Mesh -> SimulatorData
addMesh simulator mesh = simulator {
  getMeshes = mesh : getMeshes simulator
}

deleteMesh :: SimulatorData -> MeshName -> SimulatorData
deleteMesh simulator meshname = simulator {
  getMeshes = filter ((/= meshname) . name) $ getMeshes simulator
}
