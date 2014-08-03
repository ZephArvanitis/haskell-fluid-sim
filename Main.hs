import System.Exit
import Simulator
import MarchingCubes
import Control.Monad.State
import ObjectParser (name, Mesh)
import OpenCL

import Fluid

import Foreign.C.Types

main ::  IO ()
main = openCL ["cg.cl", "marchingCubes.cl", "simulation.cl"] $ runSimulator $ do
  -- Initialize
  let n = 4
  mesh <- lift $ do
    state <- initializeFluid n
    grid <- inputBuffer $ replicate (n^3) 0

    -- Run some stuff
    newState <- simulateStep state

    -- Show some output
    populateMarchingCubesGrid grid newState
    makeMesh n grid

  showMesh mesh

auxFunc :: IO ()
auxFunc = openCL ["cg.cl"] $ do
  -- our A matrix
  let diag = [[[1, 5], [3, 7]], [[2, 6], [4, 8]]]
      xp = [[[11, 15], [13, 17]], [[12, 16], [14, 18]]]
      yp = [[[5, 1], [-1, 3]], [[-2, 2], [0, 4]]]
      zp = [[[3, -1], [-3, 1]], [[-4, 0], [-2, 2]]]
  let value lst x y z  = lst !! x !! y !! z
  diagBuf <- imageBuffer 2 2 2 ImageAlpha $ value diag
  xpBuf <- imageBuffer 2 2 2 ImageAlpha $ value xp
  ypBuf <- imageBuffer 2 2 2 ImageAlpha $ value yp
  zpBuf <- imageBuffer 2 2 2 ImageAlpha $ value zp

  -- Other vector
  let vec = [[[86, 233], [51, 644]], [[-41, 249], [27, 588]]]
  vecBuf <- imageBuffer 2 2 2 ImageAlpha $ value vec
  let system = FluidCellSystem diagBuf xpBuf ypBuf zpBuf vecBuf

  -- run CG
  solution <- conjugateGradient system
  forM_ [(x, y, z) | x <- [0, 1], y <- [0, 1], z <- [0, 1]] $ \(x, y, z) -> do
    liftIO $ print (x, y, z)
    readPixel [] solution (x, y, z) >>= liftIO . print

  --openCL ["marchingCubes.cl"] $ runSimulator loop

showMesh :: Mesh -> SimulatorT OpenCL ()
showMesh mesh = do
  running <- Simulator.isRunning
  when running $ do
    Simulator.waitForNextFrame
    Simulator.update

    paused <- Simulator.isPaused
    unless paused updateSimulation

    addMesh mesh
    Simulator.draw
    deleteMesh $ name mesh

    showMesh mesh

{-
loop :: SimulatorT OpenCL ()
loop = do
  running <- Simulator.isRunning
  when running $ do
    Simulator.waitForNextFrame
    Simulator.update

    paused <- Simulator.isPaused
    unless paused updateSimulation

    mesh <- lift demoCube
    addMesh mesh
    Simulator.draw
    deleteMesh $ name mesh

    loop
-}
updateSimulation :: Monad m => SimulatorT m ()
updateSimulation = return ()
