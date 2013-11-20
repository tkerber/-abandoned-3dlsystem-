module Graphics(
  initDisplay
) where

import Primitives
import qualified LSystem as LS
import qualified Data.Map as Map
import Graphics.UI.GLUT
import Material
import Data.IORef
import Vector
import View

-- Does all calculations for the l-system as far as possible without doing
-- the actual rendering. Prevents re-calculation on every frame.
-- 
-- iterations -> expansion rule -> initial string -> length -> 
-- cylinder radius -> cylinder sides
composeLSystem :: [LS.Edge GLfloat] -> GLfloat -> Int -> Renderable
composeLSystem [] _ _ = Renderable ()
composeLSystem edges r s = Renderable $
    -- Draw a sphere for the first end
    (case head edges of
      (m, v, _) -> sphere m v r s):
    -- Draw all cylinders with a sphere on their second end.
    concat [[cylinder m vs ve r s, sphere m ve r s] | (m, vs, ve) <- edges]

-- Does the actual rendering.
display :: IORef GLfloat -> Vector3 GLfloat -> Renderable -> DisplayCallback
display rotation center lsystem = do
  clear [ColorBuffer, DepthBuffer]
  a <- get rotation
  preservingMatrix (do
    rotate a (Vector3 0 1 0)
    translate center
    render lsystem)
  swapBuffers

-- Modifies the rotation angle as needed.
idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle speed' = do
  speed <- get speed'
  angle $~! (+ speed)
  postRedisplay Nothing

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

-- Reacts to a change in window size.
-- 
-- The @ notition is very useful! Why didn't we get taught this?
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

-- Initialized the display.
-- 
-- (pitch, yaw, roll) -> iterations -> start string -> color map ->
--    expansion map
initDisplay :: (GLfloat, GLfloat, GLfloat) -> Int -> String ->
    Map.Map Char Material -> Map.Map Char String -> IO ()
initDisplay angles iter str cmap exmap = do
  getArgsAndInitialize
  
  -- Some constants about edges
  let edgeLength = 1
  let edgeRadius = 0.3
  let cylinderSides = 16
  
  -- Read variables
  let edges = LS.edgeExpand iter exmap str cmap edgeLength angles
  let lsystem = composeLSystem edges edgeRadius cylinderSides
  let center = weightedCenter edges
  let center' = toGLVector center
  let maxDist = maxDistance edges center
  let l = Light 0
  
  -- Setup window & callbacks
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  -- This title is not an oversight, it is not a placeholder, it is not
  -- temporary. It is a statement of fact.
  createWindow "Awesome stuff."
  rotation <- newIORef 0.0
  speed <- newIORef 1.0
  lastSpeed <- newIORef 0.0
  -- Why 0.7? I don't know, it looked sort of right. And who cares, you can
  -- zoom anyhow.
  let view0 = View (realToFrac maxDist * 0.7) 0.0 0.0
  view <- newIORef view0
  displayCallback $= display rotation center' lsystem
  idleCallback $= Just (idle rotation speed)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (input speed lastSpeed view)
  
  -- Lighting
  ambient l $= Color4 0 0 0 1
  diffuse l $= Color4 0.3 0.3 0.3 1
  specular l $= Color4 0 0 0 1
  position l $= Vertex4 0 0 maxDist 1
  light l $= Enabled
  lighting $= Enabled
  normalize $= Enabled
  depthFunc $= Just Lequal
  
  -- Look at it. (no, seriously, do.)
  reposition view0
  
  mainLoop

