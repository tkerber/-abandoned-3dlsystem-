module Primitives(
  Render(render),
  Renderable(Renderable),
  cylinder,
  sphere
) where

import Vector
import Material
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- Like show, but it actually shows it!
class Render a where
  render :: a -> IO ()

-- Interesting non-standard way making heterogenius objects of arbitrary
-- instances of a typeclass.
-- 
-- Of course, all information about the objects is lost, apart from the fact
-- that they are renderable. I'm not sure why there didn't seem to be a
-- standard way to do this in haskell, heterogenius collections seems like
-- a fairly common problem.
data Renderable = forall a . Render a => Renderable a

instance Render Renderable where
  render (Renderable x) = render x

instance Render a => Render [a] where
  render xs = do
    mapM render xs
    return ()

instance Render () where
  render x = return ()

-- material, the normal to the quad, then its 4 verticies.
-- (? What is the normal used for? Determining which side is in/outside?)
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
-- 
-- I later saw that GLUT had a method to render cylinders built in, but I'd
-- already written this and it would have required figuring out how to rotate
-- them as they were always drawn along the z-axis.
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

-- material -> position -> radius -> fineness (however that this implemented)
data GLUTObject =
  GLUTObject Material (Vector3 GLfloat) Object
  deriving (Show)

instance Render GLUTObject where
  render (GLUTObject m pos obj) = preservingMatrix (do
    translate pos
    material m
    renderObject Solid obj)

sphere :: Material -> Vector GLfloat -> GLfloat -> Int -> Renderable
sphere m v r n = Renderable $ GLUTObject m (toGLVector v) $
    Sphere' (realToFrac r) (fromIntegral n) (fromIntegral n)

