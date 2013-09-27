module ObjectParser (
  Mesh(..),
  Vertex(..),
  Face(..),
  MeshName,
  parseMesh
) where 

import Data.Array(Array)
import Text.Parsec.String(Parser)
import Text.Parsec.Combinator(count)
import Text.Parsec.Prim(many)
import Text.Parsec.Char(noneOf)
import Text.Parsec.Char(oneOf)
import Text.Parsec.Char(newline)
import Control.Monad(liftM)
import Text.Parsec.Combinator(many1)
import Data.Maybe(catMaybes)
import Text.Parsec.Char(char)
import Text.Parsec.Prim(parse)
import Text.Parsec.Combinator(optionMaybe)
import Data.Array(listArray)

data Mesh = Mesh {
    vertices :: Array Int Vertex,
    faces :: Array Int Face,
    normals :: Array Int Vertex,
    name :: MeshName,
    dx, dy, dz :: Float,
    sx, sy, sz :: Float,
    rx, ry, rz :: Float
} deriving Show
type MeshName = String
data Vertex = Vertex {
    x :: !Float,
    y :: !Float,
    z :: !Float
} deriving Show
data Face = Triangle Int Int Int | Quad Int Int Int Int deriving Show

vertexParser :: Parser [Vertex]
vertexParser = linesStartingWith 'v' $ do
  [x, y, z] <- count 3 $ do
    floatString <- many $ noneOf " \n"
    many $ oneOf " \n\r"
    return (read floatString)
  return $ Vertex x y z

faceParser :: Parser [Face]
faceParser = linesStartingWith 'f' $ do
  vertexList <- many1 $ do
    intString <- many1 $ noneOf " \n"
    many $ char ' '
    return (read intString)
  many $ oneOf " \n\r"
  return $ case vertexList of
    [v1, v2, v3] -> Triangle v1 v2 v3
    [v1, v2, v3, v4] -> Quad v1 v2 v3 v4
    verts -> error (show verts)
    
nameParser :: Parser [String]
nameParser = linesStartingWith 'o' $ do
  nameString <- many $ noneOf "\n"
  newline
  return nameString

linesStartingWith :: Char -> Parser a -> Parser [a]
linesStartingWith character lineParser = 
  liftM catMaybes $ maybeLine character lineParser

maybeLine :: Char -> Parser a -> Parser [Maybe a]
maybeLine character lineParser = 
  many $ do
    line <- optionMaybe (char character >> char ' ' >> lineParser)
    case line of
      Just value -> return $ Just value
      Nothing -> many (noneOf "\n") >> char '\n' >> return Nothing

parseMesh :: FilePath -> IO Mesh
parseMesh filePath = do
  fileContents <- readFile filePath
  let vertices = case parse vertexParser filePath fileContents of
        Left e -> error $ show e
        Right verts -> verts
  let faces = case parse faceParser filePath fileContents of
        Left e -> error $ show e
        Right faceList -> faceList
  let name = case parse nameParser filePath fileContents of
        Left e -> error $ show e
        Right [nameStr] -> nameStr
  return Mesh {
    dx = 0, dy = 0, dz = 0,
    rx = 0, ry = 0, rz = 0,
    sx = 1, sy = 1, sz = 1,
    vertices = listArray (1, length vertices) vertices,
    faces = listArray (1, length faces) faces,
    name = name
  }
