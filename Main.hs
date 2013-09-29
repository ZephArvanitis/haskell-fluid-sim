import Simulator
import Control.Exception (finally)
import Control.Monad.Loops (whileM_)
import Control.Monad(unless)
import Control.Monad.State

main ::  IO ()
main = flip finally Simulator.terminate $ do
  simulator <- Simulator.initialize 
  flip evalStateT simulator $
    whileM_ (gets Simulator.isRunning) $ do
      lift $ Simulator.waitForNextFrame simulator

      simulator <- get
      new <- lift $ Simulator.update simulator
      put new

      paused <- gets Simulator.isPaused
      unless paused $ lift updateSimulation
      lift $ Simulator.draw simulator

updateSimulation :: IO ()
updateSimulation = return ()
