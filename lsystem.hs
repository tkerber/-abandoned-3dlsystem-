
import qualified Data.Map as Map

pow :: Int -> (a -> a) -> (a -> a)
pow 0 f = f
pow n f = pow (n - 1) f . f

expand1 :: Ord a => Map.Map a [a] -> [a] -> [a]
expand1 map_ = concat . map expand'
  where
    expand' x
      | x `Map.member` map_ = map_ Map.! x
      | otherwise           = [x]

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

data Vector3 = Vector3 Float Float Float deriving (Eq, Ord, Show)
-- I looked at the 'correct' way of using vectors in haskell - it looked too
-- complicated for what I needed so here we are. (And OpenGL's don't do
-- addition and such)
(*#) :: Vector3 -> Int -> Vector3
(Vector3 a b c) *# x = Vector3 (a * x) (b * x) (c * x)
(+#) :: Vector3 -> Vector3 -> Vector3
(Vector3 a b c) +# (Vector3 a' b' c') = Vector3 (a + a') (b + b') (c + c')
(-#) :: Vector3 -> Vector3 -> Vector3
v1 -# v2 = v1 +# (v1 *# (-1))
-- For all intents and purposes, I am used vectors for rotation too.
angleSize :: Float
angleSize = 0.3

alpha :: Vector3
alpha = Vector3 angleSize 0 0
beta :: Vector3
beta = Vector3 0 angleSize 0
gamma :: Vector3
gamma = Vector3 0 0 angleSize

rotate :: Vector3 -> Vector3 -> Vector3
rotate vec orient = undefined
-- TODO

type Color = String
c :: Color
c = "#000000"
data Edge = Edge Color Vector3 Vector3



edges :: [Char] -> Vector3 -> Vector3 -> ([Edge], [Char])
edges [] _ _ = ([], [])
edges (x:xs) pos orient
  | x == ']'  = ([], xs)
  | x == '['  = fst recDetach `tuplePrepend` edges (snd recDetach) pos orient
  | x == '+'  = edges xs pos (orient +# alpha)
  | x == '-'  = edges xs pos (orient -# alpha)
  | x == '*'  = edges xs pos (orient +# beta)
  | x == '/'  = edges xs pos (orient -# beta)
  | x == '&'  = edges xs pos (orient +# gamma)
  | x == '|'  = edges xs pos (orient -# gamma)
  | otherwise = Edge c pos pos' `tupleCons` edges xs pos' orient
  where
    tupleCons x (y, z) = (x : y, z)
    tuplePrepend x (y, z) = (x ++ y, z)
    pos' = pos +# rotate Vector3 0 1 0 orient
    recDetach = edges xs pos orient
