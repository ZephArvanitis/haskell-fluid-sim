{-# LANGUAGE FlexibleInstances #-}
module MarchingCubes where

import Data.List( foldl', unzip3 )
import Control.Monad( forM_, forM, when )
import Control.Applicative ((<$>))
import Data.Array.MArray( newListArray )
import Data.Array.IO( IOArray )
import Data.Array( listArray, Array )

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
  let n = 40
      nC = fromIntegral n :: CInt
      radius = 20
      indices = map (gridPosition $ n + 1) [0..(n+1)^3-1]
      value = sphereValue n radius
      -- value = planeValue 0.9
      values = map value indices :: [CFloat]

  -- Initialize OpenCL and load our kernels.
  cl <- initializeOpenCL
  let filename = "marchingCubes.cl"
      kernels = ["numVertices", "generateTriangles"]
  [nvertsKernel, triKernel] <- sourceKernels cl filename kernels

  -- Allocate space for input and output
  let nCubes = n^3
  grid <- inputBuffer cl values
  nverts <- outputBuffer cl nCubes :: IO (OutputBuffer CInt)
  setKernelArgs nvertsKernel nC grid nverts

  -- Execute first kernel to get number of triangles in each cube
  nvertsOut <- runKernel cl nvertsKernel [nCubes] [1]
  nvertsList <- readKernelOutput cl nvertsOut nverts
  
  -- Do a scan through the result to create a list with one element per
  -- thread in the next step
  let (cubeNums, triNums) = unzip . scan $ zip [0..] nvertsList

  -- Set up memory for input/output things 
  let numTris = length cubeNums
      numFloats = numTris * 4

  -- fieldInputMem (grid values) already exists
  cubeInput <- inputBuffer cl cubeNums
  triInput  <- inputBuffer cl triNums
  v1Out <- outputBuffer cl numFloats :: IO (OutputBuffer CFloat)
  v2Out <- outputBuffer cl numFloats :: IO (OutputBuffer CFloat)
  v3Out <- outputBuffer cl numFloats :: IO (OutputBuffer CFloat)
  normalsOut <- outputBuffer cl numFloats :: IO (OutputBuffer CFloat)

  -- Create/initialize the kernel
  setKernelArgs triKernel nC grid cubeInput triInput v1Out v2Out v3Out normalsOut
  triOut <- runKernel cl triKernel [numTris] [1]

  let out = readKernelOutput cl triOut
  v1s <-   toVerts cubeNums nC <$> out v1Out
  v2s <-   toVerts cubeNums nC <$> out v2Out
  v3s <-   toVerts cubeNums nC <$> out v3Out
  normals <- toVerts cubeNums nC <$> out normalsOut

  let -- Convert the vertices into a single list, where each cube
      -- gets its own element. This element is a list containing, for each
      -- triangle, a tuple. The first element of this tuple is the cube
      -- number. (This is constant across all elements of the inner list.)
      -- The second element is the three vertices associated with this
      -- triangle.
      makeList cubeNum a b c = (cubeNum, [a, b, c])
      allVertices :: [[(CubeNum, [Vertex, Vertex, Vertex])]]
      allVertices = groupBy ((==) `on` fst) $ zipWith4 makeList cubeNums v1s v2s v3s

      -- collectVerts takes allVertices and using that returns two things:
      -- 1. A sparse assoc array (CubeNum, StartIndex)
      -- 2. All the vertices in order. THE indices in the previous array
      -- refer to the indices of the vertices in this order.
      collectVerts _ startInds revVerts [] = (startInds, reverse revVerts)
      collectVerts start startInds revVerts (tris:rest) =
        let cubeId = fst $ head tris
            startInds' = (cubeId, start):startInds
            revVerts' = concatMap (reverse . snd) tris ++ revVerts
          in collectVerts (start + length tris * 3) startInds' revVerts' rest

      (startInds, verts) = collectVerts 0 [] [] allVertices

  let vertices = toArray $ v1s ++ v2s ++ v3s
      mkTris a = Triangle a (a + numTris) (a + 2 * numTris)
      faces = toArray $ map mkTris [1..numTris]
      faceNormals = toArray normals
      vertexNormals = toArray . concat . replicate 3 $ normals

  -- Create the mesh from these results
  let scale = 0.005
  let mesh = Mesh{ vertices = vertices
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
