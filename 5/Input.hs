module Input (input, parse_crates) where

import Data.IntMap ((!?))
import qualified Data.IntMap as Map

-- Read this weeks input data from a file and parse it.
input f = do
  ls <- readlines f
  let (cs, ms) = split "" ls
  let moves = map parse_move ms
  let stacks = parse_stacks cs
  return (stacks, moves)
  where
    readlines = (fmap lines) . readFile

-- Split a list at the first instance of a specific value.
-- For example:
--    split 'x' "abxcd" == ("ab", "cd")
split at [] = ([], [])
split at (x : xs)
  | (at == x) = ([], xs)
  | otherwise =
      let (xs', tail) = split at xs
       in (x : xs', tail)

-- Parse a create movement "move n from x to y" into
-- a tuple (n, x, y)
parse_move :: String -> (Int, Int, Int)
parse_move move =
  let w = words move
      read' i = read (w !! i) :: Int
   in (read' 1, read' 3, read' 5)

-- Parse a layer of crates "[X] [Y]     [Z]" into
-- a map like { 1 => 'X', 2 => 'Y', 4 => 'Z' }
parse_crates str =
  let n = (length str + 1) `div` 4
      label i = str !! (4 * i - 3)
   in Map.fromList [(i, label i) | i <- [1 .. n], label i /= ' ']

-- We will never get an empty input.
parse_stacks [] = undefined
-- Base case: Intialize empty stacks.
parse_stacks [str] =
  let n = (length str + 1) `div` 4
   in Map.fromList [(n, []) | n <- [1 .. n]]
-- General case: Parse the crate on this line, and place
-- the crates on top of the stacks from the lines below.
parse_stacks (str : strs) =
  let crates = parse_crates str
      stacks = parse_stacks strs
      place_crate i stack =
        case crates !? i of
          Just crate -> (crate : stack)
          Nothing -> stack
   in Map.mapWithKey place_crate stacks
