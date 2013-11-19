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

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle speed' = do
  speed <- get speed'
  angle $~! (+ speed)
  postRedisplay Nothing

argParse :: IO ((GLfloat, GLfloat, GLfloat), Int, String,
    Map.Map Char Material, Map.Map Char String)
argParse = do
  args <- getArgs
  return (
    (
      read (args!!0) * pi / 180.0,
      read (args!!1) * pi / 180.0,
      read (args!!2) * pi / 180.0
    ),
    read (args!!3),
    args!!4,
    readMats (drop 5 args),
    readExps (drop 5 args))

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
  (angles, iter, str, cmap, exmap) <- argParse
  let edges = LS.edgeExpand iter exmap str cmap edgeLength angles
  let lsystem = composeLSystem edges edgeRadius cylinderSides
  let center = weightedCenter edges
  let center' = toGLVector center
  let maxDist = maxDistance edges center
  let l = Light 0
  
  -- Setup window & callbacks
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  createWindow "Awesome stuff."
  rotation <- newIORef 0.0
  speed <- newIORef 1.0
  lastSpeed <- newIORef 0.0
  let view0 = View (realToFrac maxDist * 0.7) 0.0 0.0
  view <- newIORef view0
  displayCallback $= display rotation center' lsystem
  idleCallback $= Just (idle rotation speed)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (input speed lastSpeed view)
  
  -- Lighting
  ambient l $= lightAmbient
  diffuse l $= lightDiffuse
  specular l $= lightSpecular
  position l $= Vertex4 0 0 maxDist 1
  light l $= Enabled
  lighting $= Enabled
  normalize $= Enabled
  depthFunc $= Just Lequal
  
  -- Rotate slightly so that is looks like it's being seen from above.
  --rotate (-20)    ((Vector3 1.0 0.0 0.0)::Vector3 GLfloat)
  
  -- Look at it.
  reposition view0
  
  mainLoop

reshape :: Size -> IO ()
reshape s@(Size w h) = do
  viewport $= ((Position 0 0), s)
  matrixMode $= Projection
  loadIdentity
  let near   = 0.001
  let far    = 1000
  let fov    = pi / 4
  let top    = near * tan fov
  let aspect = fromIntegral w / fromIntegral h
  let right  = top * aspect
  frustum (-right) right (-top) top near far
  matrixMode $= Modelview 0

-- distance, yaw, pitch
data View = View GLdouble GLdouble GLdouble deriving (Show)

reposition :: View -> IO ()
reposition (View dist yaw pitch) = do
  loadIdentity
  lookAt
    ((Vertex3
      (cos pitch * sin yaw * dist)
      (sin pitch * dist)
      (cos pitch * cos yaw * dist))::Vertex3 GLdouble)
    ((Vertex3 0 0 0)::Vertex3 GLdouble)
    ((Vector3
      (-sin pitch * sin yaw)
      (cos pitch)
      (-sin pitch * cos yaw))::Vector3 GLdouble)
  matrixMode $= Modelview 1
  return ()

zoom :: IORef View -> (GLdouble -> GLdouble) -> IO ()
zoom view' f = do
  (View dist yaw pitch) <- get view'
  let view = View (f dist) yaw pitch
  reposition view
  view' $= view
  return ()

yawRotate :: IORef View -> GLdouble -> IO ()
yawRotate view' a = do
  (View dist yaw pitch) <- get view'
  let view = View dist (yaw + a) pitch
  reposition view
  view' $= view
  return ()

pitchRotate :: IORef View -> GLdouble -> IO ()
pitchRotate view' a = do
  (View dist yaw pitch) <- get view'
  let view = View dist yaw (limit $ pitch + a)
  reposition view
  view' $= view
  return ()
  where
    limit x
      | x < -(pi / 2) = -(pi / 2)
      | x > pi / 2    = pi / 2
      | otherwise     = x

input :: IORef GLfloat -> IORef GLfloat -> IORef View -> KeyboardMouseCallback
input speed' lastSpeed' view key Down _ _ = case key of
  (Char ' ') -> (do
    lastSpeed <- get lastSpeed'
    speed <- get speed'
    speed' $= lastSpeed
    lastSpeed' $= speed
    return ())
  (Char '+') -> zoom view (/ 1.01)
  (Char '-') -> zoom view (* 1.01)
  (SpecialKey KeyLeft) -> yawRotate view (-0.04)
  (SpecialKey KeyRight) -> yawRotate view 0.04
  (SpecialKey KeyUp) -> pitchRotate view 0.04
  (SpecialKey KeyDown) -> pitchRotate view (-0.04)
  _ -> return ()
input _ _ _ _ _ _ _ = return ()

