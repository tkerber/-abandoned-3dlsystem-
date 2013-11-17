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
edges = LS.edgeExpand 3 (Map.fromList [('X', "+|Xa+|XaX/a+&&XaX-a*&&XaX/a&X/&")]) "X" 0.1

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
    --translate $ toGLVector (neg weightedCenter)
    --preservingMatrix (do
    translate $ toGLVector (neg weightedCenter)
    -- ^ -> +
    -- & -> -
    -- + -> *
    -- - -> /
    -- > -> &
    -- < -> |
    -- ^<xf^<xfx-f^>>xfx&f+>>xfx-f>x-> ==
    -- +|Xf+|XfX/f+&&XfX-f*&&XfX/f&X/&
      --preservingMatrix (do
    drawEdges edges edgeRadius 16)
        --drawSphere (fromColor 1 0 0) (Vector 0 0 0) 0.5 16)))
    --drawLSystem 1 (Map.fromList [('x', "&f*f*f|f|f/f/f&")]) "x" 0.1 0.02 8)
    --drawLSystem 1 (Map.fromList [('X', "*f")]) "X" 0.1 0.02 8)
  --drawCylinder (Vector 0 0 0) (Vector (-1) (1 / (10 ^ 28)) 0) 0.2 128
  --translate $ toGLVector weightedCenter
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
initfn = let light0 = Light 0 
         in do
               -- translate $ toGLVector (neg weightedCenter)
               ambient light0 $= lightAmbient
               diffuse light0 $= lightDiffuse
               specular light0 $= lightSpecular
               -- TODO why is this rotating with the matrix? We don't want that!
               position (Light 0) $= lightPosition
               light light0 $= Enabled
               lighting $= Enabled
               normalize $= Enabled

               depthFunc $= Just Lequal

  --             matrixMode $= Projection
--               perspective 40.0 1.0 1.0 10.0
--               matrixMode $= Modelview 0
--               lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  
--               translate ((Vector3 0.0 0.0 (-1.0))::Vector3 GLfloat)
               rotate (-20)    ((Vector3 1.0 0.0 0.0)::Vector3 GLfloat)
--               rotate (-20) ((Vector3 0.0 0.0 1.0)::Vector3 GLfloat)

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
