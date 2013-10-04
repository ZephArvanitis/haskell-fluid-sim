import Simulator
import Control.Monad.State

main ::  IO ()
main = Simulator.initialize >>= runStateT loop >> Simulator.terminate

loop :: Simulator ()
loop = do
  running <- Simulator.isRunning
  when running $ do
    Simulator.waitForNextFrame
    Simulator.update

    paused <- Simulator.isPaused
    unless paused $ liftIO updateSimulation

    Simulator.draw

    loop

updateSimulation :: IO ()
updateSimulation = return ()
