import Simulator
import MarchingCubes
import Control.Monad.State
import ObjectParser (name)
import OpenCL

main ::  IO ()
main = openCL ["marchingCubes.cl"] $ do
  runSimulator loop

loop = do
  running <- Simulator.isRunning
  when running $ do
    Simulator.waitForNextFrame
    Simulator.update

    paused <- Simulator.isPaused
    unless paused updateSimulation

    mesh <- liftIO $ demoCube cl kernels
    addMesh mesh
    Simulator.draw
    deleteMesh $ name mesh

    loop

updateSimulation :: Simulator ()
updateSimulation = return ()
