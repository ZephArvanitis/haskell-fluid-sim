{-# LANGUAGE RecordWildCards, DoAndIfThenElse #-}
module Fluid where

import Control.Monad
import Control.Applicative ((<$>))

import OpenCL
import Foreign.C.Types
import Data.List (elemIndex)

data FluidCellSystem = FluidCellSystem { diag :: FluidVector
                                       , xplus :: FluidVector
                                       , yplus :: FluidVector
                                       , zplus :: FluidVector
                                       , rhs :: FluidVector
                                       }

data FluidState = FluidState { xVels :: FluidVector
                             , yVels :: FluidVector
                             , zVels :: FluidVector
                             , pressures :: FluidVector
                             , isSolid :: FluidVector
                             }
data VectorComponent = X | Y | Z

type FluidVector = ImageBuffer CFloat

initializeFluid :: OpenCL FluidState
initializeFluid = undefined

populateMarchingCubesGrid :: InputBuffer CFloat -> FluidState -> OpenCL ()
populateMarchingCubesGrid = undefined


simulateStep :: FluidState -> OpenCL FluidState
simulateStep state@FluidState{..} = do
  -- Advect each component of velocity individually
  xVels' <- advect state X
  yVels' <- advect state Y
  zVels' <- advect state Z

  -- Apply body forces (currently just gravity)
  zVels' <- bodyForces zVels'

  -- Find pressures that keep velocities divergence-free
  let state' = state {
      xVels = xVels',
      yVels = yVels',
      zVels = zVels'
  }
  system <- computeSystem state'
  pressures' <- conjugateGradient system

  -- Update velocities using new pressures
  updateVelocity state' { pressures = pressures' }

advect :: FluidState -> VectorComponent -> OpenCL FluidVector
advect FluidState{..} component = do
  let outVelShape = xVels
      componentInt =
        case component of
          X -> 0 :: CInt
          Y -> 1
          Z -> 2
  out <- zerosWithShapeOf outVelShape
  setKernelArgs "advect" componentInt xVels yVels zVels out
  runSyncImageKernel "advect" outVelShape
  return out

bodyForces :: FluidVector -> OpenCL FluidVector
bodyForces v1 = do
  out <- zerosWithShapeOf v1
  setKernelArgs "body_forces" v1 out
  runSyncImageKernel "body_forces" v1
  return out

updateVelocity :: FluidState -> OpenCL FluidState
updateVelocity state@FluidState{..} = do
  [vx', vy', vz'] <- replicateM 3 $ zerosWithShapeOf xVels
  setKernelArgs "update_velocities_with_pressure" xVels yVels zVels pressures vx' vy' vz'
  runSyncImageKernel "update_velocities_with_pressure" xVels
  return state {
    xVels = vx',
    yVels = vy',
    zVels = vz'
  }

computeSystem :: FluidState -> OpenCL FluidCellSystem
computeSystem FluidState{..} = do
  isAir <- zerosWithShapeOf pressures
  setKernelArgs "is_air" pressures isAir
  runSyncImageKernel "is_air" pressures

  [rhs, diag, xplus, yplus, zplus] <- replicateM 5 $ zerosWithShapeOf pressures
  setKernelArgs "set_up_system" xVels yVels zVels isSolid isAir rhs diag xplus yplus zplus
  runSyncImageKernel "set_up_system" pressures
  return FluidCellSystem { diag = diag
                         , xplus = xplus
                         , yplus = yplus
                         , zplus = zplus
                         , rhs = rhs
                         }


conjugateGradient :: FluidCellSystem -> OpenCL FluidVector
conjugateGradient system = do 
  let r0 = rhs system
      p0 = r0
  x0 <- zerosWithShapeOf r0
  loop r0 p0 x0
  where 
    loop r p x = do
      alpha <- liftM2 (/) (dotProduct r r) (applyA system p >>= dotProduct p)
      x' <- addVec x p alpha
      ap <- applyA system p
      r' <- addVec r ap (-alpha)
      didConverge <- converge r'
      if didConverge 
      then return x'
      else do 
        beta <- liftM2 (/) (dotProduct r' r') (dotProduct r r)
        p' <- addVec r' p beta
        loop r' p' x'

addVec :: FluidVector -> FluidVector -> CFloat -> OpenCL FluidVector
addVec v1 v2 scale = do
  out <- zerosWithShapeOf v1
  setKernelArgs "add_vec" v1 v2 scale out

  runSyncImageKernel "add_vec" v1
  return out

applyA :: FluidCellSystem -> FluidVector -> OpenCL FluidVector
applyA FluidCellSystem{..} v = do
  out <- zerosWithShapeOf v
  setKernelArgs "apply_A" v diag xplus yplus zplus out

  runSyncImageKernel "apply_A" v
  return out

converge :: FluidVector -> OpenCL Bool
converge vec = do
  magnitude <- liftM sqrt (dotProduct vec vec)
  return $ magnitude < 0.001

zerosWithShapeOf :: FluidVector -> OpenCL FluidVector
zerosWithShapeOf demoVec = 
  imageBuffer (imageWidth demoVec)
              (imageHeight demoVec)
              (imageDepth demoVec)
              (imageType demoVec)
              zeros
  where zeros _ _ _ = 0


dotProduct :: FluidVector -> FluidVector -> OpenCL CFloat
dotProduct vec1 vec2 = do
  let width = imageWidth vec1
      height = imageHeight vec1
      depth = imageDepth vec1

  multipliedImg <- zerosWithShapeOf vec1

  setKernelArgs "elementwise_mult" vec1 vec2 multipliedImg
  multOut <- runKernel "elementwise_mult" [width, height, depth] [1, 1, 1]

  auxImg <- zerosWithShapeOf vec1

  unless (width == height && width == depth) $
    fail $ "Different width, height, depth: " ++ show [width, height, depth]

  let powersOfTwo = map (2^) [1..]
  unless (width `elem` powersOfTwo) $
    fail $ "Width not a power of two: " ++ show width

  (usefulImg, kernelOut) <- loop width multOut multipliedImg auxImg

  readPixel [kernelOut] usefulImg (0, 0, 0)
  where
    loop :: Int -- Current dimension
         -> KernelOutput -- Event to wait for
         -> FluidVector -- Image that contains useful data
         -> FluidVector  -- Image which will contain summed output
         -> OpenCL (FluidVector, KernelOutput)
    loop 1 kernelOut img _ = return (img, kernelOut)
    loop currentWidth kernelOut img aux = do
      setKernelArgs "sum_step" img aux
      let dim = currentWidth `div` 2
      nextOut <- enqueueKernel "sum_step" [dim, dim, dim] [1, 1, 1] [kernelOut]
      loop dim nextOut aux img

runSyncImageKernel :: String -> FluidVector -> OpenCL ()
runSyncImageKernel kernel img = runSyncKernel kernel [width, height, depth] [1, 1, 1]
  where
    width = imageWidth img
    height = imageHeight img
    depth = imageDepth img
