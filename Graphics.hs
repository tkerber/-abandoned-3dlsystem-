module Graphics(
  drawEdges
) where

import Primitives
import LSystem
import qualified Data.Map as Map
import Graphics.Rendering.OpenGL
import Vector

-- iterations -> expansion rule -> initial string -> length -> 
-- cylinder radius -> cylinder sides
drawEdges :: [Edge GLfloat] -> GLfloat -> Int -> IO ()
drawEdges [] _ _ = return()
drawEdges edges r s = do
  -- Draw a sphere for the first end
  case head edges of
    (m, v, _) -> drawSphere m v r s
  mapM (\(m, vs, ve) -> do
      drawCylinder m vs ve r s
      -- And then always just one.
      drawSphere m ve r s
    ) edges
  return ()
