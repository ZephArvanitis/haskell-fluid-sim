{-# LANGUAGE OverlappingInstances, ScopedTypeVariables, FlexibleInstances #-}
module OpenCL (
    Kernel,
    initializeOpenCL,
    setKernelArgs,
    sourceKernels,
    inputBuffer,
    outputBuffer,
    runKernel,
    readKernelOutput,
    OutputBuffer,
    InputBuffer,
    OpenCL
  ) where

import Control.Monad
import Control.Applicative

import Control.Parallel.OpenCL  

import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat, CInt )
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable

-- A kernel.
data Kernel = Kernel (Ptr ())
data KernelOutput = KernelOutput (Ptr ())

-- Memory buffers.
data InputBuffer a  = InputBuffer Int Int (Ptr ())

data OutputBuffer a = OutputBuffer Int Int (Ptr a) (Ptr ())



-- OpenCL Info.
data OpenCL = OpenCL {
    clPlatform :: CLPlatformID,
    clDevice :: CLDeviceID,
    clContext :: CLContext,
    clQueue :: CLCommandQueue
  }

initializeOpenCL :: IO OpenCL
initializeOpenCL = do
  -- Initialize OpenCL
  (platform:_) <- clGetPlatformIDs
  (device:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [CL_CONTEXT_PLATFORM platform] [device] putStrLn
  queue <- clCreateCommandQueue context device []
  return OpenCL {
    clPlatform = platform,
    clDevice = device,
    clContext = context,
    clQueue = queue
  }

inputBuffer :: Storable a => OpenCL -> [a] -> IO (InputBuffer a)
inputBuffer cl input = do
  arr <- newArray input 
  InputBuffer (length input) bufsize <$> clCreateBuffer (clContext cl) flags (bufsize, castPtr arr)  
  where
    flags = [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR]
    bufsize = length input * sizeOf (head input)

outputBuffer :: forall a. Storable a => OpenCL -> Int -> IO (OutputBuffer a)
outputBuffer cl size = do
  arr <- mallocArray size :: IO (Ptr a)
  OutputBuffer size bufsize arr <$> clCreateBuffer (clContext cl) flags (bufsize, nullPtr)
  where
    flags = [CL_MEM_WRITE_ONLY]
    bufsize = size * sizeOf (undefined :: a)


sourceKernels :: OpenCL -> FilePath -> [String] -> IO [Kernel]
sourceKernels cl file kernels = do
  -- Load the file and program.
  source <- readFile file
  program <- clCreateProgramWithSource (clContext cl) source
  clBuildProgram program [clDevice cl] ""

  -- Load all kernels.
  forM kernels $ \kernel ->
    Kernel <$> clCreateKernel program kernel

runKernel :: OpenCL -> Kernel -> [Int] -> [Int] -> IO KernelOutput
runKernel cl (Kernel kernel) global local  = do
  eventExec <- clEnqueueNDRangeKernel (clQueue cl) kernel global local []
  return $ KernelOutput eventExec

readKernelOutput :: Storable a => OpenCL -> KernelOutput -> OutputBuffer a -> IO [a]
readKernelOutput cl (KernelOutput exec) (OutputBuffer nels size arr mem) = do
  clEnqueueReadBuffer (clQueue cl) mem True 0 size (castPtr arr) [exec]
  peekArray nels arr

-- Variadic argument class for setKernelArgs.
-- The `args` function builds up a list of IO actions, each
-- of which should just be a `clSetKernelArgSto` call, and then
-- when it reaches the end of the argument chain with some IO a,
-- it runs all of the IO actions (in reverse).
class KernelArgs args where
  arg :: Kernel -> [IO ()] -> args

-- Build up the list of IO actions.
instance KernelArgs cl => KernelArgs (InputBuffer a -> cl) where
    arg (Kernel kernel) argStack = \(InputBuffer _ _ value) ->
      -- Prepent a new clSetKernelArgSto to the stack.
      arg (Kernel kernel) (clSetKernelArgSto kernel argNumber value:argStack)
      where
        argNumber = fromIntegral $ length argStack

instance KernelArgs cl => KernelArgs (OutputBuffer a -> cl) where
    arg (Kernel kernel) argStack = \(OutputBuffer _ _ mem value) ->
      -- Prepent a new clSetKernelArgSto to the stack.
      arg (Kernel kernel) (clSetKernelArgSto kernel argNumber value:argStack)
      where
        argNumber = fromIntegral $ length argStack

instance (KernelArgs cl, Storable a) => KernelArgs (a -> cl) where
    arg (Kernel kernel) argStack = \value ->
      -- Prepent a new clSetKernelArgSto to the stack.
      arg (Kernel kernel) (clSetKernelArgSto kernel argNumber value:argStack)
      where
        argNumber = fromIntegral $ length argStack

-- Evaluate all built-up IO actions from previous KernelArgs.
-- We allow any IO so that users do not have to specify IO () 
-- explicitly. Using the output will result in an error.
instance KernelArgs (IO a) where
  arg _ stack = do
    sequence_ $ reverse stack

    -- The output of this should never be used.
    return $ error "Cannot use output of setArgs"

-- Set arguments for a kernel.
setKernelArgs :: KernelArgs args => Kernel -> args
setKernelArgs kernel = arg kernel []
