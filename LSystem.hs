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

-- Chains f to itself n times.
pow :: Int -> (a -> a) -> (a -> a)
pow 0 f = id
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
-- +, - increase/decrease pitch
-- *, / increase/decrease yaw
-- &, | increase/decrease roll
-- 
-- All other symbols are just colored lines.

-- For all intents and purposes, I am used vectors for rotation too.
angle :: Floating a => a
angle = pi / 2

black :: Material
black = fromColor 0 0 0

color :: Char -> Material
color 'a' = fromColor 1 1 1
color 'b' = fromColor 1 0 0
color 'c' = fromColor 0 1 0
color 'd' = fromColor 0 0 1
color 'e' = fromColor 1 1 0
color 'f' = fromColor 1 0 1
color 'g' = fromColor 0 1 1
color _ = black

type Edge a = (Material, Vector a, Vector a)
--data Floating a => Edge a = Edge (Vector a) (Vector a) (Vector a) deriving (Show)

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

-- TODO roll isn't doing anything! How to get it to do stuff.
-- The chars, The position vector, and the orientation vector.
edges :: (Floating a, Eq a) => [Char] -> Vector a -> Orientation a -> a ->
    ([Edge a], [Char])
edges [] _ _ _ = ([], [])
edges (x:xs) pos orient l
  | x == ']'  = ([], xs)
  | x == '['  = fst rec `tuplePrepend` edges (snd rec) pos orient l
  | x == '+'  = edges xs pos (pitch orient angle)    l
  | x == '-'  = edges xs pos (pitch orient (-angle)) l
  | x == '*'  = edges xs pos (yaw   orient angle)    l
  | x == '/'  = edges xs pos (yaw   orient (-angle)) l
  | x == '&'  = edges xs pos (roll  orient angle)    l
  | x == '|'  = edges xs pos (roll  orient (-angle)) l
  | isLower x = (color x, pos, pos') `tupleCons` edges xs pos' orient l
  | otherwise = rec
  where
    tupleCons x (y, z) = (x : y, z)
    tuplePrepend x (y, z) = (x ++ y, z)
    pos' = pos /+ (direction orient /* l)
    rec = edges xs pos orient l

edgeExpand :: (Floating a, Eq a) => Int -> Map.Map Char String -> String ->
    a -> [Edge a]
edgeExpand n m xs l = fst $ edges (expand n m xs) v0 defaultOrientation l

