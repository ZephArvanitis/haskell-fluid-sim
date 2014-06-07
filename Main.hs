import Simulator
import MarchingCubes
import Control.Monad.State
import ObjectParser (name)
import OpenCL

import Foreign.C.Types

main ::  IO ()
main = openCL ["cg.cl"] $ do
  let value :: Int -> Int -> Int -> CFloat
      value x y z = fromIntegral x * fromIntegral y * fromIntegral z
  img <- imageBuffer 4 4 4 ImageAlpha value
  return ()
  --openCL ["marchingCubes.cl"] $ runSimulator loop

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

updateSimulation :: Monad m => SimulatorT m ()
updateSimulation = return ()
