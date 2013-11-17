module Vector(
  Vector(Vector),
  (/+),
  (/-),
  (/*),
  (//),
  neg,
  dot,
  cross,
  norm,
  len,
  orthogonal,
  rot,
  toNorm,
  toVertex,
  toGLVector,
  v0,
  (~||)
) where

-- For conversion to Vertex3 and Normal3
import Graphics.Rendering.OpenGL

-- As I'm only dealing with 3D vectors here, I am being a bit lazy and not
-- generalizing.
-- I realize there are types for vectors and classes for them which I could
-- use, but I'd have to get aquianted with them first and it seems too much
-- of a hassle.
data Vector a = Vector a a a deriving(Eq, Ord)

x :: Vector a -> a
x (Vector a _ _) = a
y :: Vector a -> a
y (Vector _ a _) = a
z :: Vector a -> a
z (Vector _ _ a) = a

instance Show a => Show (Vector a) where
  show (Vector a b c) = "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"

(/+) :: Num a => Vector a -> Vector a -> Vector a
(Vector a b c) /+ (Vector a' b' c') = Vector (a + a') (b + b') (c + c')

(/-) :: Num a => Vector a -> Vector a -> Vector a
v1 /- v2 = v1 /+ neg v2

(/*) :: Num a => Vector a -> a -> Vector a
(Vector a b c) /* x = Vector (a * x) (b * x) (c * x)

(//) :: Fractional a => Vector a -> a -> Vector a
v // x = v /* (1 / x)

neg :: Num a => Vector a -> Vector a
neg (Vector a b c) = Vector (-a) (-b) (-c)

dot :: Num a => Vector a -> Vector a -> a
(Vector a b c) `dot` (Vector a' b' c') = a * a' + b * b' + c * c'

cross :: Num a => Vector a -> Vector a -> Vector a
(Vector a b c) `cross` (Vector a' b' c') = Vector
  (b * c' - c * b')
  (c * a' - a * c')
  (a * b' - b * a')

norm :: Floating a => Vector a -> Vector a
norm v = v // len v

len :: Floating a => Vector a -> a
len v = sqrt $ v `dot` v

-- Finds a vector orthogonal to the given vector.
-- Note that the "almost parallel" check is required, as vectors almost
-- parallel to 1 0 0 will have a length of "0", due to floating point error.
orthogonal :: (Floating a, Ord a) => Vector a -> Vector a
orthogonal v
  | v ~|| Vector 1 0 0 = v `cross` Vector 0 1 0
  | otherwise          = v `cross` Vector 1 0 0

(~||) :: (Floating a, Ord a) => Vector a -> Vector a -> Bool
v1 ~|| v2 = almostParallel (norm v1) (norm v2)
  where
    almostParallel (Vector a b c) (Vector a' b' c') =
        (a ~= a' && b ~= b' && c ~= c') ||
        ((-a) ~= a' && (-b) ~= b' && (-c) ~= c')
    a ~= b = abs (a - b) <= 1.0 / (10 ^ 10)

-- A bit ugly, but it works. And no, I'm not making a matrix type just for a
-- few rotations.
-- axis -> angle -> vector -> new vector
rot :: Floating a => Vector a -> a -> Vector a -> Vector a
rot u a v = Vector
  (
    (cos' + ux * ux * cos'') * vx +
    (ux * uy * cos'' - uz * sin') * vy +
    (ux * uz * cos'' + uy * sin') * vz
  )
  (
    (uy * ux * cos'' + uz * sin') * vx +
    (cos' + uy * uy * cos'') * vy +
    (uy * uz * cos'' - ux * sin') * vz
  )
  (
    (uz * ux * cos'' - uy * sin') * vx +
    (uz * uy * cos'' + ux * sin') * vy +
    (cos' + uz * uz * cos'') * vz
  )
  where
    u' = norm u
    ux = x u'
    uy = y u'
    uz = z u'
    vx = x v
    vy = y v
    vz = z v
    sin' = sin a
    cos' = cos a
    cos'' = 1 - cos a

toNorm :: Vector a -> Normal3 a
toNorm (Vector a b c) = Normal3 a b c

toVertex :: Vector a -> Vertex3 a
toVertex (Vector a b c) = Vertex3 a b c

toGLVector :: Vector a -> Vector3 a
toGLVector (Vector a b c) = Vector3 a b c

v0 :: Num a => Vector a
v0 = Vector 0 0 0

