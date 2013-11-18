import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics
import Data.IORef
import Data.Char
import Primitives
import Material
import Vector
import System.Environment
import qualified LSystem as LS
import qualified Data.Map as Map

-- This file most mostly pilfered from some oneline tutorial.
-- 
-- Tutorials plundered:
-- 
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.126.1682&rep=rep1&type=pdf

rotationSpeed :: GLfloat
rotationSpeed = 1

display :: IORef GLfloat -> Vector3 GLfloat -> Renderable -> DisplayCallback
display rotation center lsystem = do
  clear [ColorBuffer, DepthBuffer]
  a <- get rotation
  preservingMatrix (do
    rotate a (Vector3 0 1 0)
    translate center
    render lsystem)
  swapBuffers

lightDiffuse :: Color4 GLfloat
lightDiffuse = Color4 0.2 0.2 0.2 1.0
lightAmbient :: Color4 GLfloat
lightAmbient = Color4 0.0 0.0 0.0 1.0
lightSpecular :: Color4 GLfloat
lightSpecular = Color4 0.0 0.0 0.0 1.0

idle :: IORef GLfloat -> IdleCallback
idle angle = do
  angle $~! (+ rotationSpeed)
  postRedisplay Nothing

argParse :: IO (GLfloat, Int, String, Map.Map Char Material, Map.Map Char String)
argParse = do
  args <- getArgs
  return (
    read (args!!0) * pi / 180.0,
    read (args!!1),
    args!!2,
    readMats (tail args),
    readExps (tail args))

readExps :: [String] -> Map.Map Char String
readExps = Map.fromList . readExps'
  where
    readExps' [] = []
    readExps' (x:xs)
      | length x >= 3 && take 2 (tail x) == "->" =
          (head x, drop 3 x) : readExps' xs
      | otherwise = readExps' xs

readMats :: [String] -> Map.Map Char Material
readMats = Map.fromList . readMats'
  where
    readMats' [] = []
    readMats' (x:xs)
      | length x == 8 && x!!1 == '#' =
          (x!!0, matFromHex (drop 2 x)):readMats' xs
      | otherwise = readMats' xs
    matFromHex x = fromColor
        (fromIntegral (intFromHex $ take 2 x) / 255.0)
        (fromIntegral (intFromHex $ take 2 $ drop 2 x) / 255.0)
        (fromIntegral (intFromHex $ take 2 $ drop 4 x) / 255.0)
    intFromHex [] = 0
    intFromHex (x:xs) = hexDigit x * (16 ^ length xs) + intFromHex xs
    hexDigit x
      | x >= '0' && x <= '9' = ord x - ord '0'
      | x >= 'a' && x <= 'f' = ord x - ord 'a' + 10
      | x >= 'A' && x <= 'F' = ord x - ord 'A' + 10
      | otherwise            = error "Expected hex digit."

edgeLength :: Floating a => a
edgeLength = 1

edgeRadius :: Floating a => a
edgeRadius = 0.3

cylinderSides :: Num a => a
cylinderSides = 16

-- Finds the weighted center of the edges.
weightedCenter :: [LS.Edge GLfloat] -> Vector GLfloat
weightedCenter edges = neg $
  foldr (addVecs) v0 edges // fromIntegral (length edges * 2)
  where
    addVecs (_, u, w) v = v /+ u /+ w

maxDistance :: [LS.Edge GLfloat] -> Vector GLfloat -> GLfloat
maxDistance edges center = maximum $ concat $ map radii edges
  where
    radii (_, v, u) =
      [radius (v /- center), radius (u /- center)]
    radius (Vector x y z) = sqrt (x * x + z * z + y * y)

main :: IO ()
main = do
  getArgsAndInitialize
  
  -- Read variables
  (angle, iter, str, cmap, exmap) <- argParse
  let edges = LS.edgeExpand iter exmap str cmap edgeLength angle
  let lsystem = composeLSystem edges edgeRadius cylinderSides
  let center = weightedCenter edges
  let center' = toGLVector center
  let maxDist = maxDistance edges center
  let l = Light 0
  
  -- Setup window & callbacks
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  createWindow "Awesome stuff."
  rotation <- newIORef 0.0
  displayCallback $= display rotation center' lsystem
  idleCallback $= Just (idle rotation)
  reshapeCallback $= Just reshape
  
  -- Lighting
  ambient l $= lightAmbient
  diffuse l $= lightDiffuse
  specular l $= lightSpecular
  position l $= Vertex4 0 0 (maxDist * 0.7) 1
  light l $= Enabled
  lighting $= Enabled
  normalize $= Enabled
  depthFunc $= Just Lequal
  
  -- Rotate slightly so that is looks like it's being seen from above.
  --rotate (-20)    ((Vector3 1.0 0.0 0.0)::Vector3 GLfloat)
  
  -- Look at it.
  lookAt
    ((Vertex3 0 0 ((realToFrac maxDist) * 0.7))::Vertex3 GLdouble)
    ((Vertex3 0 0 0)::Vertex3 GLdouble)
    ((Vector3 0 1 0)::Vector3 GLdouble)
  
  mainLoop

reshape s@(Size w h) = do
  viewport $= ((Position 0 0), s)
  matrixMode $= Projection
  loadIdentity
  let near   = 0.001
  let far    = 40
  let fov    = pi / 4
  let top    = near * tan fov
  let aspect = fromIntegral w / fromIntegral h
  let right  = top * aspect
  frustum (-right) right (-top) top near far
  matrixMode $= Modelview 0

