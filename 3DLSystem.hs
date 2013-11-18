import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics
import Data.IORef
import Primitives
import Material
import Vector
import qualified LSystem as LS
import qualified Data.Map as Map

-- This file most mostly pilfered from some oneline tutorial.
-- 
-- Tutorials plundered:
-- 
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.126.1682&rep=rep1&type=pdf

rotationSpeed :: GLfloat
rotationSpeed = 1

edges :: (Floating a, Eq a) => [LS.Edge a]
edges = LS.edgeExpand 3 (Map.fromList [('X', "+|Xa+|XaX/a+&&XaX-a*&&XaX/a&X/&")]) "X" (Map.fromList [('a', fromColor 1 1 1)]) 0.1 (pi/2)

edgeRadius :: Floating a => a
edgeRadius = 0.01

-- Finds the weighted center of the edges.
weightedCenter :: Vector GLfloat
weightedCenter = foldr (addVecs) v0 edges // fromIntegral (length edges * 2)
  where
    addVecs (_, u, w) v = v /+ u /+ w

maxRadius :: GLfloat
maxRadius = maximum $ concat $ map radii edges
  where
    radii (_, v, u) =
      [radius (v /- weightedCenter), radius (u /- weightedCenter)]
    radius (Vector x _ z) = sqrt (x * x + z * z)

display :: IORef GLfloat -> DisplayCallback
display angle = do
  clear [ColorBuffer, DepthBuffer]
  a <- get angle
  preservingMatrix (do
    rotate a (Vector3 0 1 0)
    translate $ toGLVector (neg weightedCenter)
    drawEdges edges edgeRadius 16)
  swapBuffers

lightDiffuse :: Color4 GLfloat
lightDiffuse = Color4 0.1 0.1 0.1 1.0
lightAmbient :: Color4 GLfloat
lightAmbient = Color4 0.1 0.1 0.1 1.0
lightSpecular :: Color4 GLfloat
lightSpecular = Color4 0.2 0.2 0.2 1.0

lightPosition :: Vertex4 GLfloat
lightPosition = Vertex4 (-1.0) 0 (-1.0) 1.0

initfn :: IO ()
initfn = let light0 = Light 0 in do
  ambient light0 $= lightAmbient
  diffuse light0 $= lightDiffuse
  specular light0 $= lightSpecular
  position (Light 0) $= lightPosition
  light light0 $= Enabled
  lighting $= Enabled
  normalize $= Enabled
  
  depthFunc $= Just Lequal
  rotate (-20)    ((Vector3 1.0 0.0 0.0)::Vector3 GLfloat)

idle :: IORef GLfloat -> IdleCallback
idle angle = do
  angle $~! (+ rotationSpeed)
  postRedisplay Nothing

main :: IO ()
main = do
  getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  createWindow "Awesome stuff."
  angle <- newIORef 0.0
  displayCallback $= display angle
  idleCallback $= Just (idle angle)
  initfn
  mainLoop
