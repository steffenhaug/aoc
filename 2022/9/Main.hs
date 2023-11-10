import Control.Monad.State
import Data.List (mapAccumL)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec hiding (State)

data Motion = Motion (Int, Int) Int deriving (Show)

input file = do
  ls <- fmap lines (readFile file)
  return (map motion ls)

motion ('L' : i) = Motion (-1,  0) (read i)
motion ('R' : i) = Motion ( 1,  0) (read i)
motion ('U' : i) = Motion ( 0,  1) (read i)
motion ('D' : i) = Motion ( 0, -1) (read i)
motion unparsable = undefined

-- Clamp a nubmer to a given range.
clamp (l, u) = max l . min u

-- Given a moved head `h`, `follow` gives you the moved tail.
follow h [] = []
follow h (k : ks) =
  let δx = fst h - fst k
      δy = snd h - snd k
      δk =
        if max_norm (δx, δy) == 2
          then (clamp' δx, clamp' δy)
          else (0, 0)
      k' = (fst k + fst δk, snd k + snd δk)
   in k' : (follow k' ks)
  where
    max_norm (x, y) = max (abs x) (abs y)
    clamp' = clamp (-1, 1)

type Rope = [(Int, Int)]

type Path = [(Int, Int)]

move :: Motion -> State (Path, Rope) ()
move (Motion δh 0) = pure ()
move (Motion δh n) = do
  (path, rope) <- get
  -- Split the rope into the first knot `k` and the
  -- rest of the knots `ks`. Move `k`, then calculate
  -- how the rest of the knots follow.
  let (k : ks) = rope
      k' = (fst k + fst δh, snd k + snd δh)
      ks' = follow k' ks
  -- Store the updated path, and the complete moved rope.
  put ((last ks') : path, k' : ks')
  -- Recursively apply the next motion to the rope.
  move (Motion δh (n - 1))

main = do
  motions <- input "input.txt"
  let proc = traverse move motions

  -- Part one.
  let rope = (take 2) (repeat (0, 0))
      path = fst $ execState proc ([], rope)
      visited = Set.fromList path
   in putStrLn $ "Part one: " ++ show (Set.size visited)

  -- Part two.
  let rope = (take 10) (repeat (0, 0))
      path = fst $ execState proc ([], rope)
      visited = Set.fromList path
   in putStrLn $ "Part two: " ++ show (Set.size visited)
