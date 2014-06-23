{-# LANGUAGE OverlappingInstances, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module OpenCL (
    OutputBuffer,
    ImageBuffer,
    InputBuffer,
    imageWidth     ,
    imageHeight    ,
    imageDepth     ,
    imageType ,
    imageMemLoc    ,
    ImageType(..),

    Kernel,
    initializeOpenCL,
    setKernelArgs,
    sourceKernels,
    inputBuffer,
    outputBuffer,
    runKernel,
    runSyncKernel,
    enqueueKernel,
    readKernelOutput,
    imageBuffer,
    readPixel,
    OpenCL,
    openCL,
    KernelOutput,
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
import Foreign.Marshal.Alloc

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- A kernel.
data Kernel = Kernel { unKernel :: Ptr () }
data KernelOutput = KernelOutput { outputEvent :: Ptr () }
type KernelName = String

-- Memory buffers.
data InputBuffer a  = InputBuffer Int Int (Ptr ())

data OutputBuffer a = OutputBuffer Int Int (Ptr a) (Ptr ())

-- Types and such to manage image memory
data ImageType = ImageRed | ImageAlpha
data ImageBuffer a = ImageBuffer {
                        imageWidth     :: Int,
                        imageHeight    :: Int,
                        imageDepth     :: Int,
                        imageType :: ImageType,
                        imageMemLoc    :: CLMem
                      }

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
  -- hardcoding my gpu cpu setup!
  (cpu:gpu1:gpu2) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  let device = gpu1
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

imageBuffer :: (Storable a, Show a)
            => Int                    -- ^ Image width.
            -> Int                    -- ^ Image height.
            -> Int                    -- ^ Image depth.
            -> ImageType              -- ^ Type of image (red or alpha, relevant for clamping)
            -> (Int -> Int -> Int -> a)  -- ^ Given x, y, z indices, generate image value at that index.
            -> OpenCL (ImageBuffer a) 
imageBuffer width height depth imageType generator = do
    context <- gets clContext
    let flags = [CL_MEM_READ_WRITE, CL_MEM_ALLOC_HOST_PTR, CL_MEM_COPY_HOST_PTR]
        imageFmt = CLImageFormat order channelType
        order = case imageType of
                  ImageRed -> CL_R
                  ImageAlpha -> CL_A
        elementSize = sizeOf (generator undefined undefined undefined)
        channelType = case elementSize of
                        1 -> CL_UNSIGNED_INT8
                        4 -> CL_FLOAT
                        _ -> error "Expecting Storable CInt8 or CFloat."
        nBytes = width * height * depth * elementSize
    memLoc <- liftIO $ allocaBytes nBytes $ \ptr -> do
      forM_ [(x, y, z) | x <- [0..width-1], y <- [0..height-1], z <- [0..depth-1]] $ \(x, y, z) -> do
        let elemOffset = x + width * y + width * height * z
        pokeByteOff ptr (elementSize * elemOffset) (generator x y z)
      clCreateImage3D context flags imageFmt width height depth 0 0 ptr
    return ImageBuffer {
      imageWidth = width,
      imageHeight = height,
      imageDepth = depth,
      imageType = imageType,
      imageMemLoc = memLoc
    }

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
runKernel kernelName global local  = enqueueKernel kernelName global local []

runSyncKernel :: KernelName -> [Int] -> [Int] -> OpenCL ()
runSyncKernel kernelName global local  = do
  KernelOutput event <- runKernel kernelName global local
  queue <- gets clQueue
  liftIO $ clEnqueueWaitForEvents queue [event]

enqueueKernel :: KernelName -> [Int] -> [Int] -> [KernelOutput] -> OpenCL KernelOutput
enqueueKernel kernelName global local outputs = do
  cl <- get
  let Just (Kernel kernel) = Map.lookup kernelName (clKernels cl)
      events = map outputEvent outputs
  eventExec <- liftIO $ clEnqueueNDRangeKernel (clQueue cl) kernel global local events
  return $ KernelOutput eventExec

readKernelOutput :: Storable a => KernelOutput -> OutputBuffer a -> OpenCL [a]
readKernelOutput (KernelOutput exec) (OutputBuffer nels size arr mem) = do
  cl <- get
  liftIO $ do
    clEnqueueReadBuffer (clQueue cl) mem True 0 size (castPtr arr) [exec]
    peekArray nels arr

-- The forall is requied for scoped type variables
readPixel :: forall a. Storable a => [KernelOutput] -> ImageBuffer a -> (Int, Int, Int) -> OpenCL a
readPixel outs image (x, y, z) = do
  queue <- gets clQueue
  let blocking = True
      size = sizeOf (undefined :: a)
  liftIO $ allocaBytes size $ \ptr -> do
    event <- clEnqueueReadImage queue (imageMemLoc image) blocking (x, y, z) (1, 1, 1) 0 0  ptr $ map outputEvent outs
    head <$> peekArray 1 (castPtr ptr)


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
      -- Prepend a new clSetKernelArgSto to the stack.
      arg kernel $ command kernel argStack value:argStack

instance KernelArgs cl => KernelArgs (OutputBuffer a -> cl) where
    arg kernel argStack (OutputBuffer _ _ mem value) =
      -- Prepend a new clSetKernelArgSto to the stack.
      arg kernel $ command kernel argStack value:argStack

instance KernelArgs cl => KernelArgs (ImageBuffer a -> cl) where
    arg kernel argStack imgBuffer =
      -- Prepend a new clSetKernelArgSto to the stack.
      arg kernel $ command kernel argStack (imageMemLoc imgBuffer):argStack

instance (KernelArgs cl, Storable a) => KernelArgs (a -> cl) where
    arg kernel argStack value =
      -- Prepend a new clSetKernelArgSto to the stack.
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
