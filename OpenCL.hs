{-# LANGUAGE OverlappingInstances, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving #-}
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
    OpenCL,
    openCL
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Applicative

import Control.Parallel.OpenCL  

import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat, CInt )
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- A kernel.
data Kernel = Kernel { unKernel :: Ptr () }
data KernelOutput = KernelOutput (Ptr ())
type KernelName = String

-- Memory buffers.
data InputBuffer a  = InputBuffer Int Int (Ptr ())

data OutputBuffer a = OutputBuffer Int Int (Ptr a) (Ptr ())



-- OpenCL Info.
data OpenCLData = OpenCLData {
    clPlatform :: CLPlatformID,
    clDevice :: CLDeviceID,
    clContext :: CLContext,
    clQueue :: CLCommandQueue,
    clKernels :: Map String Kernel
  }

newtype OpenCL a = OpenCL { unOpenCL :: StateT OpenCLData IO a }
                   deriving (Monad, MonadIO, Functor, Applicative, MonadState OpenCLData)

{-

-- These lines are automatically added by GeneralizedNewtypeDeriving

instance Monad OpenCL where
  return = OpenCL . return
  OpenCL st >>= f = OpenCL $ st >>= (unOpenCL . f)

instance MonadIO OpenCL where
  liftIO = OpenCL . liftIO

-}

openCL :: [FilePath] -> OpenCL a -> IO a
openCL filenames action = do
  initState <- initializeOpenCL filenames
  evalStateT (unOpenCL action) initState 

initializeOpenCL :: [FilePath] -> IO OpenCLData
initializeOpenCL filenames = do
  -- Initialize OpenCL
  (platform:_) <- clGetPlatformIDs
  (device:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [CL_CONTEXT_PLATFORM platform] [device] putStrLn
  queue <- clCreateCommandQueue context device []
  let cl = OpenCLData {
    clPlatform = platform,
    clDevice = device,
    clContext = context,
    clQueue = queue,
    clKernels = Map.fromList []
  }
  kernels <- concat <$> mapM (sourceKernels cl) filenames
  return $ cl { clKernels = Map.fromList kernels }

inputBuffer :: Storable a => [a] -> OpenCL (InputBuffer a)
inputBuffer input = do
  arr <- liftIO $ newArray input 
  cl <- get
  InputBuffer (length input) bufsize <$> liftIO (clCreateBuffer (clContext cl) flags (bufsize, castPtr arr))
  where
    flags = [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR]
    bufsize = length input * sizeOf (head input)

outputBuffer :: forall a. Storable a => Int -> OpenCL (OutputBuffer a)
outputBuffer size = do
  arr <- liftIO $ mallocArray size :: OpenCL (Ptr a)
  cl <- get
  OutputBuffer size bufsize arr <$> liftIO (clCreateBuffer (clContext cl) flags (bufsize, nullPtr))
  where
    flags = [CL_MEM_WRITE_ONLY]
    bufsize = size * sizeOf (undefined :: a)


sourceKernels :: OpenCLData -> FilePath -> IO [(String, Kernel)]
sourceKernels cl file = do
  -- Load the file and program.
  source <- readFile file
  program <- clCreateProgramWithSource (clContext cl) source
  clBuildProgram program [clDevice cl] ""

  -- Load all kernels.
  kernels <- clCreateKernelsInProgram program
  names <- mapM clGetKernelFunctionName kernels 
  return $ zip names $ map Kernel kernels

runKernel :: KernelName -> [Int] -> [Int] -> OpenCL KernelOutput
runKernel kernelName global local  = do
  cl <- get
  let Just (Kernel kernel) = Map.lookup kernelName (clKernels cl)
  eventExec <- liftIO $ clEnqueueNDRangeKernel (clQueue cl) kernel global local []
  return $ KernelOutput eventExec

readKernelOutput :: Storable a => KernelOutput -> OutputBuffer a -> OpenCL [a]
readKernelOutput (KernelOutput exec) (OutputBuffer nels size arr mem) = do
  cl <- get
  liftIO $ do
    clEnqueueReadBuffer (clQueue cl) mem True 0 size (castPtr arr) [exec]
    peekArray nels arr

-- Variadic argument class for setKernelArgs.
-- The `args` function builds up a list of IO actions, each
-- of which should just be a `clSetKernelArgSto` call, and then
-- when it reaches the end of the argument chain with some IO a,
-- it runs all of the IO actions (in reverse).
class KernelArgs args where
  arg :: KernelName -> [OpenCL ()] -> args

-- Build up the list of IO actions.
instance KernelArgs cl => KernelArgs (InputBuffer a -> cl) where
    arg kernel argStack (InputBuffer _ _ value) =
      -- Prepent a new clSetKernelArgSto to the stack.
      arg kernel $ command kernel argStack value:argStack

instance KernelArgs cl => KernelArgs (OutputBuffer a -> cl) where
    arg kernel argStack (OutputBuffer _ _ mem value) =
      -- Prepent a new clSetKernelArgSto to the stack.
      arg kernel $ command kernel argStack value:argStack

instance (KernelArgs cl, Storable a) => KernelArgs (a -> cl) where
    arg kernel argStack value =
      -- Prepent a new clSetKernelArgSto to the stack.
      arg kernel $ command kernel argStack value:argStack

command :: Storable a => KernelName -> [OpenCL ()] -> a -> OpenCL ()
command kernel argStack value = do
  kernel <- gets (unKernel . fromJust . Map.lookup kernel . clKernels)
  liftIO $ clSetKernelArgSto kernel argNumber value
  where
    argNumber = fromIntegral $ length argStack

-- Evaluate all built-up IO actions from previous KernelArgs.
-- We allow any IO so that users do not have to specify IO () 
-- explicitly. Using the output will result in an error.
instance KernelArgs (OpenCL a) where
  arg _ stack = do
    sequence_ $ reverse stack

    -- The output of this should never be used.
    return $ error "Cannot use output of setArgs"

-- Set arguments for a kernel.
setKernelArgs :: KernelArgs args => KernelName -> args
setKernelArgs kernel = arg kernel []
