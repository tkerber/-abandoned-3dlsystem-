module Primitives(
  drawCylinder,
  drawSphere
) where
-- I am not working with OpenGL's vector type for as much of this program as
-- I can, as my grasp of monads is shaky at best, and that of OpenGL even
-- worse.
import Vector
import Material
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT

-- First the normal to the quad, then its 4 verticies.
type Quad = (Vector GLfloat, Vector GLfloat, Vector GLfloat, Vector GLfloat,
  Vector GLfloat)


-- start point, end point, radius, sides
cylinder :: Vector GLfloat -> Vector GLfloat -> GLfloat -> Int -> [Quad]
cylinder s e r' n = [(
    norm i,
    r i /+ s,
    r i /+ e,
    r (i + 1) /+ e,
    r (i + 1) /+ s
  ) | i <- [0..n - 1]]
  where
    d = e /- s
    r 0 = orth /* (r' / len orth)
    r i = rot d (2 * pi * (fromIntegral i) / n') (r 0)
    norm i = rot d (2 * pi * ((fromIntegral i) + 0.5) / n') (r 0)
    orth = orthogonal d
    n' = fromIntegral n

drawQuads :: Material -> [Quad] -> IO ()
drawQuads m q = do
  mapM (\(n, v1, v2, v3, v4) -> do
    renderPrimitive Quads $ do
      material m
      normal (toNorm n)
      vertex (toVertex v1)
      vertex (toVertex v2)
      vertex (toVertex v3)
      vertex (toVertex v4)) q
  return ()

-- I later saw that GLUT had a method to render cylinders built in, but I'd
-- already written this and it would have required figuring out how to rotate
-- them as they were always drawn along the z-axis.
drawCylinder :: Material -> Vector GLfloat -> Vector GLfloat -> GLfloat -> Int -> IO ()
drawCylinder m s e r n = drawQuads m $ cylinder s e r n

-- material -> position -> radius -> fineness (however that this implemented)
drawSphere :: Material -> Vector GLfloat -> GLfloat -> Int -> IO()
drawSphere m v r n = do
  preservingMatrix (do
    translate (toGLVector v)
    material m
    GLUT.renderObject GLUT.Solid (GLUT.Sphere' (realToFrac r) (fromIntegral n) (fromIntegral n)))

-- a = Vector 1 2 3 `dot` Vector 3 2 1
--cylinder :: Vector GLfloat -> Vector GLfloat -> GLfloat -> Int ->
--    [[Vector GLfloat]]
--cylinder from to width i = undefined
