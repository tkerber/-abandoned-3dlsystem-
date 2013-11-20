module View(
  View(View),
  input,
  reposition
) where

import Graphics.UI.GLUT
import Data.IORef

-- distance, yaw, pitch
data View = View GLdouble GLdouble GLdouble deriving (Show)

-- Calculates your new position.
reposition :: View -> IO ()
reposition (View dist yaw pitch) = do
  loadIdentity
  lookAt
    ((Vertex3
      (cos pitch * sin yaw * dist)
      (sin pitch * dist)
      (cos pitch * cos yaw * dist))::Vertex3 GLdouble)
    ((Vertex3 0 0 0)::Vertex3 GLdouble)
    ((Vector3
      (-sin pitch * sin yaw)
      (cos pitch)
      (-sin pitch * cos yaw))::Vector3 GLdouble)
  matrixMode $= Modelview 1
  return ()

-- Zooms in. Note that this is an exponential zoom, i.e. the distance from
-- the object increases/decreases by a constant factor.
-- 
-- (Actually, it isn't the function itself that does that...)
zoom :: IORef View -> (GLdouble -> GLdouble) -> IO ()
zoom view' f = do
  (View dist yaw pitch) <- get view'
  let view = View (f dist) yaw pitch
  reposition view
  view' $= view
  return ()

-- Rotates the observer's yaw.
yawRotate :: IORef View -> GLdouble -> IO ()
yawRotate view' a = do
  (View dist yaw pitch) <- get view'
  let view = View dist (yaw + a) pitch
  reposition view
  view' $= view
  return ()

-- Rotates the observer's pitch. Capped at 90 & -90 degrees.
-- Things get confusing when they are upside down.
pitchRotate :: IORef View -> GLdouble -> IO ()
pitchRotate view' a = do
  (View dist yaw pitch) <- get view'
  let view = View dist yaw (limit $ pitch + a)
  reposition view
  view' $= view
  return ()
  where
    limit x
      | x < -(pi / 2) = -(pi / 2)
      | x > pi / 2    = pi / 2
      | otherwise     = x

-- Handes user keyboard input.
-- +/- Zoom (exponentially) in or out.
-- left/right - rotate left or right.
-- up/down - rotate up or down.
-- space - pause/resume auto rotation.
input :: IORef GLfloat -> IORef GLfloat -> IORef View -> KeyboardMouseCallback
input speed' lastSpeed' view key Down _ _ = case key of
  (Char ' ') -> (do
    lastSpeed <- get lastSpeed'
    speed <- get speed'
    speed' $= lastSpeed
    lastSpeed' $= speed
    return ())
  (Char '+') -> zoom view (/ 1.01)
  (Char '-') -> zoom view (* 1.01)
  (SpecialKey KeyLeft) -> yawRotate view (-0.04)
  (SpecialKey KeyRight) -> yawRotate view 0.04
  (SpecialKey KeyUp) -> pitchRotate view 0.04
  (SpecialKey KeyDown) -> pitchRotate view (-0.04)
  _ -> return ()
-- How many underscores is that?
input _ _ _ _ _ _ _ = return ()


