module Main where

import Control.Parallel.OpenCL  
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat, CInt )
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.List( foldl' )
import Control.Monad( forM_, forM )

main :: IO ()
main = do
  -- Initialize OpenCL
  (platform:_) <- clGetPlatformIDs
  (device:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [CL_CONTEXT_PLATFORM platform] [device] print
  queue <- clCreateCommandQueue context device []
  
  -- Initialize Kernels
  kernelSource <- readFile "marchingcubes.cl"
  program <- clCreateProgramWithSource context kernelSource
  clBuildProgram program [device] ""
  -- First pass: calculate number of vertices in each cell
  kernel <- clCreateKernel program "numVertices"
  -- Scan occurs between first and second passes
  -- Second pass: generate the triangles
  
  -- Initialize the field values
  let n = 100
      radius = 20
      indices = [(x, y, z) | x <- [0..n-1], y <- [0..n-1], z <- [0..n-1]]
      value :: (Int, Int, Int) -> CFloat
      value (x, y, z) = if distance > radius^2 then 0 else 1
        where distance = (x - n `div` 2)^2
                       + (y - n `div` 2)^2
                       + (z - n `div` 2)^2
      values = map value indices

  -- Size of workgroups and such
  let bufSize = n^3 * sizeOf (undefined :: CFloat)
      numWorkGroups = (n - 1)^3
      outputSize = numWorkGroups * sizeOf (undefined :: CInt)

  -- Allocate space for input and output
  input <- newArray values
  output <- mallocArray outputSize :: IO (Ptr CInt)

  -- allocate memory for input and output on device
  fieldInputMem <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (bufSize, castPtr input)  
  numVertsMem <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (outputSize, nullPtr)

  clSetKernelArgSto kernel 0 fieldInputMem
  clSetKernelArgSto kernel 1 numVertsMem
  clSetKernelArgSto kernel 2 (fromIntegral n :: CInt)

  print "Began running"
  
  -- Execute Kernel
  eventExec <- clEnqueueNDRangeKernel queue kernel [numWorkGroups] [1] []
  
  -- Get Result
  eventRead <- clEnqueueReadBuffer queue numVertsMem True 0 outputSize (castPtr output) [eventExec]
  
  result <- peekArray numWorkGroups output
  print $ take 20 $ filter (>0) result


  -- Scan though cubes and count triangles
  -- cube #, triangle # within cube
  let scan :: [(CInt, CInt)] -> [(CInt, CInt)]
      scan [] = []
      scan ((0, _):xs) = scan xs
      scan ((result, index):xs) = (index, result `div` 3 - 1):scan ((result-3, index):xs)

  let scannedthing = scan $ zip result [0..]
  print $ take 20 scannedthing
