import Simulator
import MarchingCubes
import Control.Monad.State
import ObjectParser (name)
import OpenCL

main ::  IO ()
main = openCL ["marchingCubes.cl"] $ runSimulator loop

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

updateSimulation :: Simulator ()
updateSimulation = return ()
