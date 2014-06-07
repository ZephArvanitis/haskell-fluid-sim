module Fluid where

import OpenCL

dotProduct :: ImageBuffer CFloat -> ImageBuffer CFloat -> OpenCL CFloat
dotProduct vec1 vec2 = do
  let zeros _ _ _ = 0
      width = imageWidth vec1
      height = imageHeight vec1
      depth = imageDepth vec1
  multipliedImg <- imageBuffer width height depth (imageType vec1) zeros

  setKernelArgs "elementwise_mult" vec1 vec2 multipliedImg
  multOut <- runKernel "elementwise_mult" [width, height, depth] [1, 1, 1]

  auxImg <- imageBuffer width height depth (imageType vec1) zeros

  when (not $ width == height && width == depth) $
    fail "Different width, height, depth: " ++ show [width, height, depth]

  let powersOfTwo = map (2^) [1..]
  when (not $ width `elem` powersOfTwo) $
    fail "Width not a power of two: " ++ show width

  let Just nSteps = elemIndex width powersOfTwo

  (usefulImg, kernelOut) <- loop nSteps multOut multipliedImg auxImg

  value <- readPixel kernelOut usefulImg (0, 0, 0)
  return value
  where
    loop :: Int -- Current dimension
         -> KernelOutput -- Event to wait for
         -> ImageBuffer CFloat -- Image that contains useful data
         -> ImageBuffer CFloat  -- Image which will contain summed output
         -> OpenCL (ImageBuffer CFloat, KernelOutput)
    loop 1 kernelOut img _ = return (img, kernelOut)
    loop currentWidth kernelOut img aux = do
      setKernelArgs "sum_step" img aux
      let dim = currentWidth / 2
      nextOut <- enqueueKernel "sum_step" [dim, dim, dim] [1, 1, 1] [kernelOut]
      loop dim nextOut aux img
