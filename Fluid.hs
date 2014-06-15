module Fluid where

import Control.Monad
import Control.Applicative ((<$>))

import OpenCL
import Foreign.C.Types
import Data.List (elemIndex)

data FluidCellMatrix = FluidCellMatrix { diag :: ImageBuffer CFloat
                                       , xplus :: ImageBuffer CFloat
                                       , yplus :: ImageBuffer CFloat
                                       , zplus :: ImageBuffer CFloat
                                       }

conjugateGradient :: FluidCellMatrix -> ImageBuffer CFloat -> OpenCL (ImageBuffer CFloat)
conjugateGradient matrixA vecB = do 
  let r0 = vecB
      p0 = r0
  x0 <- zerosWithShapeOf vecB
  loop r0 p0 x0
  where 
    loop r p x = do
      alpha <- liftM2 (/) (dotProduct r r) (applyA p >>= dotProduct p)
      x' <- addVec x p alpha
      ap <- applyA p
      r' <- addVec r ap (-alpha)
      didConverge <- converge r'
      if didConverge 
      then return x'
      else do 
        beta <- liftM2 (/) (dotProduct r' r') (dotProduct r r)
        p' <- addVec r' p beta
        loop r' p' x'
        

zerosWithShapeOf :: ImageBuffer CFloat -> OpenCL (ImageBuffer CFloat)
zerosWithShapeOf demoVec = 
  imageBuffer (imageWidth demoVec)
              (imageHeight demoVec)
              (imageDepth demoVec)
              (imageType demoVec)
              zeros
  where zeros _ _ _ = 0


dotProduct :: ImageBuffer CFloat -> ImageBuffer CFloat -> OpenCL CFloat
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

  snd <$> readPixel kernelOut usefulImg (0, 0, 0)
  where
    loop :: Int -- Current dimension
         -> KernelOutput -- Event to wait for
         -> ImageBuffer CFloat -- Image that contains useful data
         -> ImageBuffer CFloat  -- Image which will contain summed output
         -> OpenCL (ImageBuffer CFloat, KernelOutput)
    loop 1 kernelOut img _ = return (img, kernelOut)
    loop currentWidth kernelOut img aux = do
      setKernelArgs "sum_step" img aux
      let dim = currentWidth `div` 2
      nextOut <- enqueueKernel "sum_step" [dim, dim, dim] [1, 1, 1] [kernelOut]
      loop dim nextOut aux img
