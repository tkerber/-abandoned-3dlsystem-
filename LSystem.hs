module LSystem(
  Edge,
  expand,
  edges,
  edgeExpand,
) where

import Material
import Vector
import Data.Char
import qualified Data.Map as Map

getWithDefault :: Ord a => Map.Map a b -> (a -> b) -> a -> b
getWithDefault m f x
  | x `Map.member` m = m Map.! x
  | otherwise        = f x

-- Chains f to itself n times.
pow :: Int -> (a -> a) -> (a -> a)
pow 0 f = id
pow n f = pow (n - 1) f . f

-- Does a single L System expansion.
expand1 :: Ord a => Map.Map a [a] -> [a] -> [a]
expand1 map_ = concat . map (getWithDefault map_ (\x -> [x]))

-- Does n L Syste expansions.
expand :: Ord a => Int -> Map.Map a [a] -> [a] -> [a]
expand n map_ = pow n (expand1 map_)

-- Special characters:
-- 
-- [ - starts a detach.
-- ] - ends a detch.
-- +, - increase/decrease pitch
-- *, / increase/decrease yaw
-- &, | increase/decrease roll
-- 
-- All other symbols are just colored lines.

black :: Material
black = fromColor 0 0 0

color :: Map.Map Char Material -> Char -> Material
color m = getWithDefault m (\_ -> fromColor 1 1 1)

type Edge a = (Material, Vector a, Vector a)

-- The orientation is given by the pitch yaw and roll vectors.
-- these should be orthogonal unit vectors.
type Orientation a = (Vector a, Vector a, Vector a)

defaultOrientation :: Num a => Orientation a
defaultOrientation = (Vector 1 0 0, Vector 0 0 1, Vector 0 1 0)

direction :: Orientation a -> Vector a
direction (_, _, x) = x

-- Rotate around the pitch axis
pitch :: Floating a => Orientation a -> a -> Orientation a
pitch (p, y, r) a = (p, rot p a y, rot p a r)
-- Rotate around the yaw axis
yaw :: Floating a => Orientation a -> a -> Orientation a
yaw (p, y, r) a   = (rot y a p, y, rot y a r)
-- Rotate around the roll axis
roll :: Floating a => Orientation a -> a -> Orientation a
roll (p, y, r) a  = (rot r a p, rot r a y, r)

-- The chars, The position vector, and the orientation vector.
edges :: (Floating a, Eq a) => [Char] -> Map.Map Char Material -> a ->
    (a, a, a) -> [Edge a]
edges xs cmap l (p, y, r) = fst $ edges' v0 defaultOrientation xs
  where
    -- edges' returns a tuple of [Edge], [Char], with the second being the
    -- characters left over after completion (reattaching).
    edges' _ _ [] = ([], [])
    edges' pos orient (x:xs)
      | x == ']'  = ([], xs)
      | x == '['  = fst rec `tuplePrepend` edges' pos orient (snd rec)
      | x == '+'  = edges' pos (pitch orient p)    xs
      | x == '-'  = edges' pos (pitch orient (-p)) xs
      | x == '*'  = edges' pos (yaw   orient y)    xs
      | x == '/'  = edges' pos (yaw   orient (-y)) xs
      | x == '&'  = edges' pos (roll  orient r)    xs
      | x == '|'  = edges' pos (roll  orient (-r)) xs
      | isLower x = (color cmap x, pos, pos') `tupleCons`
          edges' pos' orient xs
      | otherwise = rec
      where
        pos' = pos /+ (direction orient /* l)
        rec = edges' pos orient xs
    tupleCons x (y, z) = (x : y, z)
    tuplePrepend x (y, z) = (x ++ y, z)

edgeExpand :: (Floating a, Eq a) => Int -> Map.Map Char String -> String ->
    Map.Map Char Material -> a -> (a, a, a) -> [Edge a]
edgeExpand n m xs = edges (expand n m xs)

