import Simulator
import Control.Exception (finally)
import Control.Monad.Loops (whileM_)
import Control.Monad.State

main ::  IO ()
main = flip finally Simulator.terminate $ do
  simulator <- Simulator.initialize 
  flip evalStateT simulator $
    whileM_ (gets Simulator.isRunning) $ do
      simulator <- get
      lift $ Simulator.waitForNextFrame simulator

      new <- lift $ Simulator.update simulator
      put new

      simulator <- get
      paused <- gets Simulator.isPaused
      unless paused $ lift updateSimulation
      lift $ Simulator.draw simulator

updateSimulation :: IO ()
updateSimulation = return ()
