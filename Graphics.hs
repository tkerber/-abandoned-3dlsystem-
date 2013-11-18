module Graphics(
  composeLSystem
) where

import Primitives
import LSystem
import qualified Data.Map as Map
import Graphics.Rendering.OpenGL
import Vector

-- iterations -> expansion rule -> initial string -> length -> 
-- cylinder radius -> cylinder sides
composeLSystem :: [Edge GLfloat] -> GLfloat -> Int -> Renderable
composeLSystem [] _ _ = Renderable ()
composeLSystem edges r s = Renderable $
    -- Draw a sphere for the first end
    (case head edges of
      (m, v, _) -> sphere m v r s):
    -- Draw all cylinders with a sphere on their second end.
    concat [[cylinder m vs ve r s, sphere m ve r s] | (m, vs, ve) <- edges]
--drawEdges edges r s = do
--  -- Draw a sphere for the first end
--  case head edges of
--    (m, v, _) -> drawSphere m v r s
--  mapM (\(m, vs, ve) -> do
--      drawCylinder m vs ve r s
--      -- And then always just one.
--      drawSphere m ve r s
--    ) edges
--  return ()
