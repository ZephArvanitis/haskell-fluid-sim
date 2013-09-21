import Simulator
import Control.Exception (finally)
import Control.Monad.Loops (whileM_)
import Control.Monad(when)
 
main ::  IO ()
main = do
  flip finally Simulator.terminate $ do
    simulator <- Simulator.initialize 
    whileM_ (Simulator.isRunning simulator) $ do
      Simulator.waitForNextFrame simulator
      simulator <- Simulator.update simulator
      paused <- Simulator.isPaused simulator
      when (not paused) updateSimulation
      Simulator.draw simulator
