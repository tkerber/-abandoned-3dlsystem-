import Graphics.Rendering.OpenGL
import Graphics
import Data.Char
import Material
import System.Environment
import qualified Data.Map as Map

-- Parses the arguments. Not the most elegant of command line options, but
-- it works and I'm lazy.
-- 
-- args -> ((pitch, yaw, roll), iterations, init string, color map,
--    material map)
argParse :: [String] -> ((GLfloat, GLfloat, GLfloat), Int, String,
    Map.Map Char Material, Map.Map Char String)
argParse args = (
    (
      read (args!!0) * pi / 180.0,
      read (args!!1) * pi / 180.0,
      read (args!!2) * pi / 180.0
    ),
    read (args!!3),
    args!!4,
    readMats (drop 5 args),
    readExps (drop 5 args))

-- Reads the expansion rules from the arg strings.
readExps :: [String] -> Map.Map Char String
readExps = Map.fromList . readExps'
  where
    readExps' [] = []
    readExps' (x:xs)
      | length x >= 3 && take 2 (tail x) == "->" =
          (head x, drop 3 x) : readExps' xs
      | otherwise = readExps' xs

-- Reads the color map from the arg strings. Ugly hex stuff.
readMats :: [String] -> Map.Map Char Material
readMats = Map.fromList . readMats'
  where
    readMats' [] = []
    readMats' (x:xs)
      | length x == 8 && x!!1 == '#' =
          (x!!0, matFromHex (drop 2 x)):readMats' xs
      | otherwise = readMats' xs
    matFromHex x = fromColor
        (fromIntegral (intFromHex $ take 2 x) / 255.0)
        (fromIntegral (intFromHex $ take 2 $ drop 2 x) / 255.0)
        (fromIntegral (intFromHex $ take 2 $ drop 4 x) / 255.0)
    intFromHex [] = 0
    intFromHex (x:xs) = hexDigit x * (16 ^ length xs) + intFromHex xs
    hexDigit x
      | x >= '0' && x <= '9' = ord x - ord '0'
      | x >= 'a' && x <= 'f' = ord x - ord 'a' + 10
      | x >= 'A' && x <= 'F' = ord x - ord 'A' + 10
      | otherwise            = error "Expected hex digit."

main :: IO ()
main = do
  args <- getArgs
  if length args < 5 then
    putStrLn help
  else
    (let (angles, iter, str, cmap, exmap) = argParse args in
      initDisplay angles iter str cmap exmap)
  return ()

help :: String
help = unlines [
    "Usage:",
    "",
    "./3DLSystems PITCH YAW ROLL ITER INIT COLORS... RULES...",
    "",
    "PITCH  - The angle to be used for pitch rotations (+/-)",
    "YAW    - The angle to be used for yaw rotations (*//)",
    "ROLL   - The angle to be used for roll rotations (&/|)",
    "ITER   - The number of iterations to go through.",
    "INIT   - The initial string to expand.",
    "COLORS - The colors in which edges should be draw for a character.",
    "         Each color has the form 'C#RRGGBB' where C is the character",
    "         and RRGGBB is a hex triplet for the color. If no color is",
    "         specified, the character will be rendered as white.",
    "RULES  - The expansion rules for the L-system. Each rule has the form",
    "         C->STRING where C is the character to expand and STRING is ",
    "         the result of expanding it.",
    "",
    "All angles are specified in degrees. It's hard to enter pi's as a",
    "parameter.",
    "",
    "The following special characters exist:",
    "",
    "+ - Increases the pitch angle",
    "- - Decreases the pitch angle",
    "* - Increases the yaw angle",
    "/ - Decreases the yaw angle",
    "& - Increases the roll angle",
    "| - Decreases the roll angle",
    "[ - Detaches",
    "] - Ends a detaches sequence",
    "",
    "Any character which isn't one of these characters or a lowercase",
    "character will be ignored by the rendering. They can be useful as",
    "variables during expansion."
  ]

