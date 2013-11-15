module Graphics(
  drawLSystem
) where

import Cylinder
import LSystem
import qualified Data.Map as Map
import Graphics.Rendering.OpenGL
import Vector

-- iterations -> expansion rule -> initial string -> length -> 
-- cylinder radius -> cylinder sides
drawLSystem :: Int -> Map.Map Char String -> String -> GLfloat -> GLfloat ->
    Int -> IO ()
drawLSystem n m init l r s = do
  mapM (\(_, vs, ve) -> do
      drawCylinder vs ve r s
    ) (edgeExpand n m init l)
  return ()
