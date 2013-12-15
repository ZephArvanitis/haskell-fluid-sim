module Main where

import Control.Parallel.OpenCL  
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat, CInt )
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
  let n = 100 :: CInt
      indices = [(x, y, z) | x <- [0..n-1], y <- [0..n-1], z <- [0..n-1]]
      radius = 20
      value :: (CInt, CInt, CInt) -> CFloat
      value (x, y, z) = if distance > radius^2 then 0 else 1
        where distance = (x - n `div` 2)^2
                       + (y - n `div` 2)^2
                       + (z - n `div` 2)^2
      values = map value indices

  input <- newArray values
  
  print $ length values
  print $ take 10 $ filter (>0) values

  -- allocate memory for input and output on device
  let bufsize = n^3 * sizeOf (undefined :: CFloat)
  fieldInputMem <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (bufsize, castPtr input)  
  let outputbufsize = (n-1)^3 * sizeOf (undefined :: CFloat)
  numVertsMem <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (outputbufsize, nullPtr)

  let numWorkGroups = (n - 1)^3

  clSetKernelArgSto kernel 0 fieldInputMem
  clSetKernelArgSto kernel 1 numVertsMem
  clSetKernelArgSto kernel 2 n
  
  -- Execute Kernel
  eventExec <- clEnqueueNDRangeKernel queue kernel [numWorkGroups] [1] []
  
  -- Get Result
  eventRead <- clEnqueueReadBuffer queue numVertsMem True 0 (n^3) (castPtr input) [eventExec]
  
  result <- peekArray (n^3) input
  print $ take 20 result
  -- putStrLn $ "Result  array =  " ++ show result
