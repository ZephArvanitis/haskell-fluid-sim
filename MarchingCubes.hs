{-# LANGUAGE FlexibleInstances #-}
module MarchingCubes where

import Data.List( foldl', unzip3, groupBy, zipWith4, zip3, nub, sort )
import Data.List.Split
import Data.Function( on )
import Control.Monad( forM_, forM, when )
import Control.Applicative ((<$>))
import Data.Array.MArray( newListArray, getBounds, readArray, writeArray, freeze )
import Data.Array.IO( IOArray )
import Data.Array( listArray, Array, array, elems )
import Foreign.C.Types( CInt, CFloat )

import ObjectParser
import OpenCL

type CubeNum = CInt
type TriangleNum = CInt
type TriangleInCube = CInt

-- Functions to generate field values:
sphereValue n radius (x, y, z) = if distance > radius^2 then 0 else 1
  where distance = (x - n `div` 2)^2
                 + (y - n `div` 2)^2
                 + (z - n `div` 2)^2

planeValue zCutoff (_, _, z) 
  | fromIntegral z < zCutoff = 1
  | otherwise   = 0

demoCube :: IO Mesh
demoCube = do
  
  -- Initialize the field values for a sphere.
  let n = 10
      nC = fromIntegral n :: CInt
      radius = 4
      indices = map (gridPosition $ n + 1) [0..(n+1)^3-1]
      value = sphereValue n radius
      --value = planeValue 0.9
      values = map value indices :: [CFloat]

  -- Initialize OpenCL and load our kernels.
  cl <- initializeOpenCL
  let filename = "marchingCubes.cl"
      kernels = ["numVertices", "genVerts", "vertexPositions"]
  [nvertsKernel, genVerts, vertexPositions] <- sourceKernels cl filename kernels

  -- Allocate space for input and output
  let nCubes = n^3
  grid <- inputBuffer cl values
  nverts <- outputBuffer cl nCubes :: IO (OutputBuffer CInt)
  cubeIndices <- outputBuffer cl nCubes :: IO (OutputBuffer CInt)
  setKernelArgs nvertsKernel nC grid nverts cubeIndices

  -- Execute first kernel to get number of triangles in each cube
  nvertsOut <- runKernel cl nvertsKernel [nCubes] [1]
  nvertsList <- readKernelOutput cl nvertsOut nverts
  cubeIndicesList <- readKernelOutput cl nvertsOut cubeIndices
  
  -- Do a scan through the result to create a list with one element per
  -- thread in the next step
  let (tuples, vertIds) = unzip . scan $ zip (zip [(0::CInt)..] cubeIndicesList) nvertsList
      (cubeIds, cubeInds) = unzip tuples
      -- Compute the starting index for each cube
      startingIndices = countStarts cubeIds 0 0 :: [CInt]
      countStarts remainingIds next ind =
        case remainingIds of
          [] -> []
          id:more -> 
            case compare id next of
              GT -> ind:countStarts (id:more) (next + 1) ind
              EQ -> ind:countStarts more (next + 1) (ind + 1)
              LT -> countStarts more next (ind + 1)

  -- Setting up for genVerts
  let numVerts = length vertIds
  cubeIdInput <- inputBuffer cl cubeIds
  cubeIndInput <- inputBuffer cl cubeIndicesList
  vertIdInput <- inputBuffer cl vertIds
  startingVertInput <- inputBuffer cl startingIndices
  globalVertInds <- outputBuffer cl numVerts :: IO (OutputBuffer CInt)

  -- Generate vertices
  setKernelArgs genVerts nC cubeIdInput cubeIndInput vertIdInput startingVertInput globalVertInds
  genVertsOut <- runKernel cl genVerts [numVerts] [1]

  -- Rename vertices to one through k (k = length of unique vertices)
  vertArray <- readKernelOutput cl genVertsOut globalVertInds >>= toMArray

  let loop ind counter backwardsAccum = do
        (_, len) <- getBounds vertArray
        if ind > len
        then return $ reverse backwardsAccum
        else do
          value <- readArray vertArray ind
          if value + 1 == fromIntegral ind
          then do
            writeArray vertArray ind counter
            loop (ind + 1) (counter + 1) (value:backwardsAccum)
          else do
            readArray vertArray (fromIntegral value + 1) >>= writeArray vertArray ind
            loop (ind + 1) counter backwardsAccum

  -- oh gods im so sorry im sorry im sorry
  previousGlobalIndices <- loop 1 1 [] :: IO [CInt]
  renamedVerts <- freeze vertArray

  let maxVertInd = maximum $ elems renamedVerts

  -- Generate vertex positions!!!
  let numUniqueVerts = fromIntegral maxVertInd
  globalVertIds <- inputBuffer cl previousGlobalIndices
  vertPosOut <- outputBuffer cl (numUniqueVerts * 4) :: IO (OutputBuffer CFloat)

  -- Generate vertices
  setKernelArgs vertexPositions nC grid globalVertIds cubeIdInput cubeIndInput vertIdInput vertPosOut
  vertexPositionsOut <- runKernel cl vertexPositions [numUniqueVerts] [1]

  -- Convert float3s to vectors
  positions <- readKernelOutput cl vertexPositionsOut vertPosOut
  let toVert [x, y, z, _] = Vertex (realToFrac x) (realToFrac y) (realToFrac z)
      vertices = map toVert $ chunksOf 4 positions
      toTri [v1, v2, v3] = Triangle (fromIntegral v1) (fromIntegral v2) (fromIntegral v3)
      faces = map toTri $ chunksOf 3 $ elems renamedVerts
  print numVerts
  print $ length vertices

  -- Get face normals
  let (vertexNormals, faceNormals) = computeNormals faces vertices

  -- Create the mesh from these results
  let scale = 0.1
  let mesh = Mesh{ vertices = toArray vertices
                 , faces = toArray faces
                 , faceNormals = toArray faceNormals
                 , vertexNormals = toArray vertexNormals
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

-- Given a list of (item, count), generate a list where
-- for each such tuple, we output `count` number of tuples,
-- (thing, 0), (thing, 1), ..., (thing, count - 1)
scan :: [(a, CInt)] -> [(a, CInt)]
scan things = case things of
  [] -> []
  ((_, 0):xs) -> scan xs
  ((thing, n):xs) -> zip (replicate (fromIntegral n) thing) [0..n-1] ++ scan xs
