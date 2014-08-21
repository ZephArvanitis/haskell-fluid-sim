{-# LANGUAGE DoAndIfThenElse #-}
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
  state <- lift $ initializeFluid n
  grid <- lift $ inputBuffer $ replicate (n^3) 0
  loop state grid
  where
    n = 8
    loop state grid = do
      running <- Simulator.isRunning
      state' <- if not running
               then return state
               else do
                 Simulator.waitForNextFrame
                 Simulator.update

                 paused <- Simulator.isPaused
                 newState <- if paused
                             then return state
                             else do
                               newState <- lift $ simulateStep state
                               lift $ populateMarchingCubesGrid grid state
                               return newState

                 mesh <- lift $ makeMesh n grid

                 addMesh mesh
                 Simulator.draw
                 deleteMesh $ name mesh

                 -- Run some stuff
                 return newState
      loop state' grid
