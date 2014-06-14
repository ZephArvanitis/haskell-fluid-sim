module Fluid where

import Control.Monad
import Control.Applicative ((<$>))

import OpenCL
import Foreign.C.Types
import Data.List (elemIndex)

dotProduct :: ImageBuffer CFloat -> ImageBuffer CFloat -> OpenCL CFloat
dotProduct vec1 vec2 = do
  let zeros _ _ _ = 0 :: CFloat 
      width = imageWidth vec1
      height = imageHeight vec1
      depth = imageDepth vec1

  multipliedImg <- imageBuffer width height depth (imageType vec1) zeros

  setKernelArgs "elementwise_mult" vec1 vec2 multipliedImg
  multOut <- runKernel "elementwise_mult" [width, height, depth] [1, 1, 1]

  auxImg <- imageBuffer width height depth (imageType vec1) zeros

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
