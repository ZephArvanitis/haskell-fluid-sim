module Main where

import Control.Parallel.OpenCL  
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat )
import Foreign.Marshal.Array( peekArray, withArray, newArray)
import Data.List( foldl' )
import Control.Monad( forM_, forM )

main :: IO ()
main = do
  -- Initialize OpenCL
  (platform:_) <- clGetPlatformIDs
  (device:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [CL_CONTEXT_PLATFORM platform] [device] print
  queue <- clCreateCommandQueue context device []
  
  -- Initialize Kernel
  kernelSource <- readFile "marchingcubes.cl"
  program <- clCreateProgramWithSource context kernelSource
  clBuildProgram program [device] ""
  kernel <- clCreateKernel program "cubes"
  
  -- Initialize parameters
  -- let original = [0 .. 20] :: [CFloat]
  --     elemSize = sizeOf (0 :: CFloat)
  --     vecSize = elemSize * length original
  -- putStrLn $ "Original array = " ++ show original
  let n = 100
      indices = [(x, y, z) | x <- [0..n-1], y <- [0..n-1], z <- [0..n-1]]
      radius = 20
      value :: (Int, Int, Int) -> CFloat
      value (x, y, z) = if distance > radius^2 then 0 else 1
        where distance = (x - n `div` 2)^2
                         + (y - n `div` 2)^2
                         + (z - n `div` 2)^2
      values = map value indices

  input <- newArray values
  
  print $ length values
  print $ take 10 $ filter (>0) values

  let bufsize = n^3 * sizeOf (undefined :: CFloat)
  fieldInputMem <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (bufsize, castPtr input)  
  numVertsMem <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (bufsize, nullPtr)

  clSetKernelArgSto kernel 0 fieldInputMem
  clSetKernelArgSto kernel 1 numVertsMem
  
  -- Execute Kernel
  let numWorkGroups = n^3
  eventExec <- clEnqueueNDRangeKernel queue kernel [numWorkGroups] [1] []
  
  -- Get Result
  eventRead <- clEnqueueReadBuffer queue numVertsMem True 0 (n^3) (castPtr input) [eventExec]
  
  result <- peekArray (n^3) input
  print $ take 20 result
  -- putStrLn $ "Result  array =  " ++ show result
