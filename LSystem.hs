module LSystem(
  expand,
  edges,
  edgeExpand
) where

import Vector
import qualified Data.Map as Map

-- Chains f to itself n times.
pow :: Int -> (a -> a) -> (a -> a)
pow 0 f = f
pow n f = pow (n - 1) f . f

-- Does a single L System expansion.
expand1 :: Ord a => Map.Map a [a] -> [a] -> [a]
expand1 map_ = concat . map expand'
  where
    expand' x
      | x `Map.member` map_ = map_ Map.! x
      | otherwise           = [x]

-- Does n L Syste expansions.
expand :: Ord a => Int -> Map.Map a [a] -> [a] -> [a]
expand n map_ = pow n (expand1 map_)

-- Special characters:
-- 
-- [ - starts a detach.
-- ] - ends a detch.
-- +, - increase/decrease alpha
-- *, / increase/decrease beta
-- &, | increase/decrease gamma
-- 
-- All other symbols are just colored lines.

-- For all intents and purposes, I am used vectors for rotation too.
angleSize :: Floating a => a
angleSize = pi / 2

alpha :: Floating a => Vector a
alpha = Vector angleSize 0 0
beta :: Floating a => Vector a
beta  = Vector 0 angleSize 0
gamma :: Floating a => Vector a
gamma = Vector 0 0 angleSize

-- TODO: figure out the relation between this rotation and that around an
-- axis in Vector.hs. If possible eliminate this one.
rotate :: (Floating a, Eq a) => Vector a -> Vector a -> Vector a
rotate (Vector 0 0 0) vec = vec
rotate (Vector alpha 0 0) (Vector a b c) =
  Vector a (cos' * b - sin' * c) (sin' * b + cos' * c)
  where
    cos' = cos alpha
    sin' = sin alpha
rotate (Vector 0 beta 0) (Vector a b c) =
  Vector (cos' * a + sin' * c) b ((-sin') * a + cos' * c) 
  where
    cos' = cos beta
    sin' = sin beta
rotate (Vector 0 0 gamma) (Vector a b c) =
  Vector (cos' * a - sin' * b) (sin' * a + cos' * b) c
  where
    cos' = cos gamma
    sin' = sin gamma
rotate (Vector a b c) v = rotate alpha $ rotate beta $ rotate gamma v
  where
    alpha = Vector a 0 0
    beta  = Vector 0 b 0
    gamma = Vector 0 0 c

black :: Floating a => Vector a
black = Vector 0 0 0

color :: Floating a => Char -> Vector a
color 'a' = Vector 1 0 0
color 'b' = Vector 0 1 0
color 'c' = Vector 0 0 1
color 'd' = Vector 1 1 0
color 'e' = Vector 1 0 1
color 'f' = Vector 0 1 1
color _ = black
-- TODO add the Edge = (Vector a) (Vector a) (Vector a) (color, start, end)
-- type back in.
type Edge a = (Vector a, Vector a, Vector a)
--data Floating a => Edge a = Edge (Vector a) (Vector a) (Vector a) deriving (Show)



edges :: (Floating a, Eq a) => [Char] -> Vector a -> Vector a -> a ->
    ([Edge a], [Char])
edges [] _ _ _ = ([], [])
edges (x:xs) pos orient l
  | x == ']'  = ([], xs)
  | x == '['  = fst recDetach `tuplePrepend`
      edges (snd recDetach) pos orient l
  | x == '+'  = edges xs pos (orient /+ alpha) l
  | x == '-'  = edges xs pos (orient /- alpha) l
  | x == '*'  = edges xs pos (orient /+ beta) l
  | x == '/'  = edges xs pos (orient /- beta) l
  | x == '&'  = edges xs pos (orient /+ gamma) l
  | x == '|'  = edges xs pos (orient /- gamma) l
  | otherwise = (color x, pos, pos') `tupleCons` edges xs pos' orient l
  where
    tupleCons x (y, z) = (x : y, z)
    tuplePrepend x (y, z) = (x ++ y, z)
    pos' = pos /+ rotate orient (Vector 0 l 0)
    recDetach = edges xs pos orient l

edgeExpand :: (Floating a, Eq a) => Int -> Map.Map Char String -> String ->
    a -> [Edge a]
edgeExpand n m xs l = fst $ edges (expand n m xs) v0 v0 l

