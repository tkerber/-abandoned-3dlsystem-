module Material(
  Material(Material),
  fromColor,
  material
) where

import Graphics.Rendering.OpenGL

-- Material color shininess
data Material = Material (Color4 GLfloat) GLfloat

material :: Material -> IO ()
material (Material c s) = do
  materialDiffuse   Front $= c
  materialAmbient   Front $= c
  materialSpecular  Front $= c
  materialShininess Front $= s
  
  materialDiffuse   Back  $= c
  materialSpecular  Back  $= c
  materialShininess Back  $= s

fromColor :: GLfloat -> GLfloat -> GLfloat -> Material
fromColor a b c = Material (Color4 a b c 1) 0.0
