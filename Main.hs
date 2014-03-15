import Simulator
import MarchingCubes
import Control.Monad.State
import ObjectParser (name)
import OpenCL

main ::  IO ()
main = do
  cl <- initializeOpenCL
  kernels <- marchingCubeKernels cl
  runSimulator $ loop cl kernels

loop cl kernels = do
  running <- Simulator.isRunning
  when running $ do
    Simulator.waitForNextFrame
    Simulator.update

    paused <- Simulator.isPaused
    unless paused $ liftIO updateSimulation

    mesh <- liftIO $ demoCube cl kernels
    addMesh mesh
    Simulator.draw
    deleteMesh $ name mesh

    loop cl kernels

updateSimulation :: IO ()
updateSimulation = return ()
