module Primitives(
  Render(render),
  Renderable(Renderable),
  cylinder,
  sphere
) where
-- I am not working with OpenGL's vector type for as much of this program as
-- I can, as my grasp of monads is shaky at best, and that of OpenGL even
-- worse.
import Vector
import Material
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT

class Render a where
  render :: a -> IO ()

data Renderable = forall a . (Render a, Show a) => Renderable a

instance Show Renderable where
  show (Renderable x) = show x

instance Render Renderable where
  render (Renderable x) = render x

instance Render a => Render [a] where
  render xs = do
    mapM render xs
    return ()

instance Render () where
  render x = return ()

-- material, the normal to the quad, then its 4 verticies.
data Quad =
  Quad Material (Normal3 GLfloat) (Vertex3 GLfloat) (Vertex3 GLfloat)
    (Vertex3 GLfloat) (Vertex3 GLfloat)
  deriving (Show)

instance Render Quad where
  render (Quad m n v1 v2 v3 v4) = renderPrimitive Quads $ do
    material m
    normal n
    vertex v1
    vertex v2
    vertex v3
    vertex v4

-- material, start point, end point, radius, sides
cylinder :: Material -> Vector GLfloat -> Vector GLfloat -> GLfloat -> Int ->
    Renderable
cylinder m s e r' n = Renderable $ [Quad
    m
    (toNorm (norm i))
    (toVertex (r i /+ s))
    (toVertex (r i /+ e))
    (toVertex (r (i + 1) /+ e))
    (toVertex (r (i + 1) /+ s))
   | i <- [0..n - 1]]
  where
    d = e /- s
    r 0 = orth /* (r' / len orth)
    r i = rot d (2 * pi * (fromIntegral i) / n') (r 0)
    norm i = rot d (2 * pi * ((fromIntegral i) + 0.5) / n') (r 0)
    orth = orthogonal d
    n' = fromIntegral n

--drawQuads :: Material -> [Quad] -> IO ()
--drawQuads m q = do
--  mapM (\(n, v1, v2, v3, v4) -> do
--    renderPrimitive Quads $ do
--      material m
--      normal (toNorm n)
--      vertex (toVertex v1)
--      vertex (toVertex v2)
--      vertex (toVertex v3)
--      vertex (toVertex v4)) q
--  return ()

-- I later saw that GLUT had a method to render cylinders built in, but I'd
-- already written this and it would have required figuring out how to rotate
-- them as they were always drawn along the z-axis.
--drawCylinder :: Material -> Vector GLfloat -> Vector GLfloat -> GLfloat -> Int -> IO ()
--drawCylinder m s e r n = drawQuads m $ cylinder s e r n

-- material -> position -> radius -> fineness (however that this implemented)
data GLUTObject =
  GLUTObject Material (Vector3 GLfloat) GLUT.Object
  deriving (Show)

instance Render GLUTObject where
  render (GLUTObject m pos obj) = preservingMatrix (do
    translate pos
    material m
    GLUT.renderObject GLUT.Solid obj)

sphere :: Material -> Vector GLfloat -> GLfloat -> Int -> Renderable
sphere m v r n = Renderable $ GLUTObject m (toGLVector v) $
    GLUT.Sphere' (realToFrac r) (fromIntegral n) (fromIntegral n)

--drawSphere :: Material -> Vector GLfloat -> GLfloat -> Int -> IO()
--drawSphere m v r n = do
--  preservingMatrix (do
--    translate (toGLVector v)
--    material m
--    GLUT.renderObject GLUT.Solid (GLUT.Sphere' (realToFrac r) (fromIntegral n) (fromIntegral n)))

-- a = Vector 1 2 3 `dot` Vector 3 2 1
--cylinder :: Vector GLfloat -> Vector GLfloat -> GLfloat -> Int ->
--    [[Vector GLfloat]]
--cylinder from to width i = undefined
