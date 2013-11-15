import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Cylinder
import Vector
import Graphics
import qualified Data.Map as Map

-- This file most mostly pilfered from some oneline tutorial.

display :: DisplayCallback
display = do
  clear [ColorBuffer, DepthBuffer]
  -- Todo: This l system makes some wierd thick lines. Why?
  drawLSystem 3 (Map.fromList [('f', "f&f|f|f&f")]) "&f" 0.1 0.05 32
  --drawCylinder (Vector 0 0 0) (Vector (-1) (1 / (10 ^ 28)) 0) 0.2 128
  swapBuffers

lightDiffuse :: Color4 GLfloat
lightDiffuse = Color4 5.0 5.0 5.0 0.5

lightPosition :: Vertex4 GLfloat
lightPosition = Vertex4 0 0 (-1.0) 0.0

initfn :: IO ()
initfn = let light0 = Light 0 
         in do diffuse light0 $= lightDiffuse
               position light0 $= lightPosition
               light light0 $= Enabled
               lighting $= Enabled

               depthFunc $= Just Lequal

--               matrixMode $= Projection
 --              perspective 40.0 1.0 1.0 10.0
 --              matrixMode $= Modelview 0
 --              lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  
--               translate ((Vector3 0.0 0.0 (-1.0))::Vector3 GLfloat)
--               rotate 40    ((Vector3 1.0 0.0 0.0)::Vector3 GLfloat)
               --rotate (-20) ((Vector3 0.0 0.0 1.0)::Vector3 GLfloat)

main :: IO ()
main = do
  getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
  createWindow "Awesome stuff."
  displayCallback $= display
  initfn
  mainLoop
