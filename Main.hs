import Simulator
import MarchingCubes
import Control.Monad.State
import ObjectParser (name)

main ::  IO ()
main = runSimulator loop

loop :: Simulator ()
loop = do
  running <- Simulator.isRunning
  when running $ do
    Simulator.waitForNextFrame
    Simulator.update

    paused <- Simulator.isPaused
    unless paused $ liftIO updateSimulation

    mesh <- liftIO demoCube
    addMesh mesh
    Simulator.draw
    deleteMesh $ name mesh

    loop

updateSimulation :: IO ()
updateSimulation = return ()
