import Control.Monad.State
import Data.Map (Map, keys, (!))
import qualified Data.Map as Map
import Text.Parsec hiding (State)

import Aoc (nat, pairs, printall)

input f = fmap (parse paths f) (readFile f)
  where
    paths = many path
    path  = sepBy point (string " -> ") <* newline
    point = do
      x <- nat
      char ','
      y <- nat
      pure (x, y)

data Material = Rock | Air | Sand deriving (Eq, Show)

type Cave = Map (Int, Int) Material

-- Draw rock between two points.
draw v w cave =
  if v == w
    then cave'
    else draw v' w cave'
  where
    v'    = (fst v + dx, snd v + dy)
    cave' = Map.insert v Rock cave
    dx    = signum $ fst w - fst v
    dy    = signum $ snd w - snd v

-- State of the falling sand in the cave.
data Sandfall
  = Sandfall
      { sandpos :: (Int, Int)
      , cave    :: Cave
      , ground  :: Int
      }

-- Functions to curry for use with modify in the State monad.
set_pos xy sandfall = sandfall { sandpos = xy }
set_material xy mat sandfall = sandfall { cave = Map.insert xy mat (cave sandfall) }

-- Follow the path of a sand block, and return where it lands.
step :: State Sandfall (Int, Int)
step = do
  Sandfall (x, y) cave ground  <- get

  let down       = (x,     y + 1)
      down_left  = (x - 1, y + 1)
      down_right = (x + 1, y + 1)

  let material (x, y)
        | y >= ground = Rock
        | otherwise   = Map.findWithDefault Air (x, y) cave

  if material down == Air then do
    modify $ set_pos down
    step
  else if material down_left == Air then do
    modify $ set_pos down_left
    step
  else if material down_right == Air then do
    modify $ set_pos down_right
    step
  else do
    modify $ set_material (x, y) Sand
    pure (x, y)

-- Simlate sand falling into the given cave while counting.
simulate thecave = evalState (fall 1) start
  where
    floor = maximum (map snd (keys thecave)) + 2
    start = Sandfall (500, 0) thecave floor
    fall i = do
      modify $ set_pos (500, 0)
      landed_at <- step
      -- If we block the sand source, we stop.
      if landed_at == (500, 0) then
        pure i
      else
        fall (i+1)

main = do
  Right paths <- input "input.txt"

  let cave = foldl (flip . uncurry $ draw) Map.empty (paths >>= pairs)
      n    = simulate cave

  print n
