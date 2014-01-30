{-# LANGUAGE ExistentialQuantification #-}
module MarchingCubes where

import Control.Parallel.OpenCL  
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat, CInt )
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.List( foldl', unzip3 )
import Control.Monad( forM_, forM )
import Control.Applicative ((<$>))
import Data.Array.MArray( newListArray )
import Data.Array.IO( IOArray )
import Data.Array( listArray, Array )

import ObjectParser

type CubeNum = CInt
type TriangleNum = CInt
type TriangleInCube = CInt

-- Functions to generate field values:
sphereValue :: Int -> Int -> (Int, Int, Int) -> CFloat
sphereValue n radius (x, y, z) = if distance > radius^2 then 0 else 1
  where distance = (x - n `div` 2)^2
                 + (y - n `div` 2)^2
                 + (z - n `div` 2)^2

planeValue :: Float -> (Int, Int, Int) -> CFloat
planeValue zCutoff (_, _, z) 
  | fromIntegral z < zCutoff = 1
  | otherwise   = 0

demoCube :: IO Mesh
demoCube = do
  -- Initialize OpenCL
  (platform:_) <- clGetPlatformIDs
  (device:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [CL_CONTEXT_PLATFORM platform] [device] putStrLn
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
  let n = 50
      radius = 20
      indices = map (gridPosition $ n + 1) [0..(n+1)^3-1]
      value = sphereValue n radius --planeValue 0.9

      values = map value indices

  -- Size of workgroups and such
  let bufSize = (n+1)^3 * sizeOf (undefined :: CFloat)
      numWorkGroups = n^3
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

  -- Execute first kernel to get number of triangles in each cube
  eventExec <- clEnqueueNDRangeKernel queue kernel [numWorkGroups] [1] []
  
  -- Get Result out of that kernel
  eventRead <- clEnqueueReadBuffer queue numVertsMem True 0 outputSize (castPtr output) [eventExec]
  result <- peekArray numWorkGroups output

  let indicesOfCubes = map (gridPosition n) [0..n^3-1]
  forM_ (zip indicesOfCubes result) $ \(idx, res) -> do
    putStr $ show idx
    putStr " "
    print res

  -- Do a scan through the result to create a list with one element per
  -- thread in the next step
  let (cubeNums, triNums) = unzip . scan $ zip [0..] result

  -- Set up memory for input/output things 
  let numTris = length cubeNums
      numTrisInputSize = numTris * sizeOf (undefined :: CInt)
      numVertsOutputSize = 4 * numTris * sizeOf (undefined :: CFloat)

  -- fieldInputMem (grid values) already exists
  cubeIDinput <- newArray cubeNums
  triIDinput <- newArray triNums
  v1output <- mallocArray numVertsOutputSize :: IO (Ptr CFloat)
  v2output <- mallocArray numVertsOutputSize :: IO (Ptr CFloat)
  v3output <- mallocArray numVertsOutputSize :: IO (Ptr CFloat)
  faceNormalOutput <- mallocArray numVertsOutputSize :: IO (Ptr CFloat)


  let mkInputBuf size input = clCreateBuffer context 
        [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (size, castPtr input)  
      mkOutputBuf size = clCreateBuffer context 
        [CL_MEM_WRITE_ONLY] (size, nullPtr)

  -- Input memory creating buffers
  cubeIDmem <- mkInputBuf numTrisInputSize cubeIDinput
  triIDmem <- mkInputBuf numTrisInputSize triIDinput

  v1mem <- mkOutputBuf numVertsOutputSize
  v2mem <- mkOutputBuf numVertsOutputSize
  v3mem <- mkOutputBuf numVertsOutputSize
  faceNormalMem  <- mkOutputBuf numVertsOutputSize

  -- Create/initialize the kernel
  kernel2 <- clCreateKernel program "generateTriangles"

  clSetKernelArgSto kernel2 0 (fromIntegral n :: CInt)
  clSetKernelArgSto kernel2 1 fieldInputMem
  clSetKernelArgSto kernel2 2 cubeIDmem
  clSetKernelArgSto kernel2 3 triIDmem
  clSetKernelArgSto kernel2 4 v1mem
  clSetKernelArgSto kernel2 5 v2mem
  clSetKernelArgSto kernel2 6 v3mem
  clSetKernelArgSto kernel2 7 faceNormalMem

  -- Execute the kernel to get corners of all the triangles
  eventExec <- clEnqueueNDRangeKernel queue kernel2 [numTris] [1] []


  -- Get results! finally
  eventReadV1 <- clEnqueueReadBuffer queue v1mem True 0 numVertsOutputSize (castPtr v1output) [eventExec]
  eventReadV2 <- clEnqueueReadBuffer queue v2mem True 0 numVertsOutputSize (castPtr v2output) [eventExec]
  eventReadV3 <- clEnqueueReadBuffer queue v3mem True 0 numVertsOutputSize (castPtr v3output) [eventExec]
  eventReadNorm <- clEnqueueReadBuffer queue faceNormalMem True 0 numVertsOutputSize (castPtr faceNormalOutput) [eventExec]

  let numFloats = numTris * 4
  let n' = fromIntegral n :: CInt
  v1result <-   toVerts cubeNums n' <$> peekArray numFloats v1output
  v2result <-   toVerts cubeNums n' <$> peekArray numFloats v2output
  v3result <-   toVerts cubeNums n' <$> peekArray numFloats v3output
  normresult <- toVerts cubeNums n' <$> peekArray numFloats faceNormalOutput

  -- Join vertex lists into a master vertex list One list to rule them
  -- all..
  let allVerts = toArray $ v1result ++ v2result ++ v3result
  -- Generate faces; this could be fun.
  let faces = toArray [Triangle a (a + length cubeNums) (a + 2 * length cubeNums) | 
        a <- [1..length cubeNums]]
  let faceNormals = toArray normresult
  -- HACK for vertex normals
  let vertexNormals = toArray . concat . replicate 3 $ normresult

  {-
  print  result
  print v1result
  print v2result
  print v3result
  -}

  -- Create the mesh from these results
  let scale = 0.1
  let mesh = Mesh{ vertices = allVerts
                 , faces = faces
                 , faceNormals = faceNormals
                 , vertexNormals = vertexNormals
                 , name = "Coolest Mesh Ever"
                 , dx = 0 , dy = 0 , dz = 1
                 , sx = scale , sy = scale , sz = scale
                 , rx = 0 , ry = 0 , rz = 0
                 }

  return mesh

toVerts :: [CubeNum] -> CubeNum -> [CFloat] -> [Vertex]
toVerts [] _ [] = []
toVerts (currCube:restCubes) n (x:y:z:_:rest) = 
          Vertex 
          (f $ x + fromIntegral x') 
          (f $ y + fromIntegral y') 
          (f $ z + fromIntegral z') : toVerts restCubes n rest
  where
   f = realToFrac
   (x', y', z') = gridPosition n currCube

toMArray :: [a] -> IO (IOArray Int a)
toMArray things = newListArray (1, length things) things

toArray :: [a] -> Array Int a
toArray things = listArray (1, length things) things
  
gridIndex :: Integral a => a -> (a, a, a) -> a
gridIndex n (x, y, z) = z + n * y + n^2 * x

gridPosition :: Integral a => a -> a -> (a, a, a)
gridPosition n num = (x, y, z) where
  z = num `mod` n
  y = ((num - z) `div` n) `mod` n
  x = (num - z - n * y) `div` n^2

-- Scan though cubes and count triangles
scan :: [(CubeNum, TriangleInCube)]  -- ^ the cube and # of triangles in it
     -> [(CubeNum, TriangleInCube)]  -- ^ one element per triangle:
                                    -- cube number, which
                                    -- triangle in the cube,
-- base case!
scan [] = []

-- no triangles in the cube, then ignore
scan ((_, 0):xs) = scan xs

-- if there are triangles in the cube, generate a list element
-- and decrease the number of triangles when recursing
scan ((cubeNum, triWithinCube):xs) = 
  (cubeNum, triWithinCube `div` 3 - 1) :
  scan ((cubeNum, triWithinCube - 3):xs)
