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

  doMagicToGetImgStuff
  where
    loop :: Int
         -> KernelOutput -- Event to wait for
         -> ImageBuffer CFloat -- Image that contains useful data
         -> ImageBuffer CFloat  -- Image which will contain summed output
         -> OpenCL (ImageBuffer CFloat, KernelOutput)
    loop 0 kernelOut img _ = return (img, kernelOut)
    loop steps kernelOut img aux = do
      enqueueSomeKernels
      recurseOrSomething
      return victory
  

