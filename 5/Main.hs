import Control.Monad.State
import Data.IntMap (IntMap, elems, insert, (!))
import qualified Data.IntMap as Map
import Input

type Stack = [Char]

type Stacks = IntMap Stack

-- A crane move is a function that moves crates from one stack to another.
type Move = Int -> (Stack, Stack) -> (Stack, Stack)

-- Apply a CrateMover motion in the state monad.
apply_move :: Move -> (Int, Int, Int) -> (State Stacks) ()
apply_move move (n, from, to) = do
  stacks <- get
  let f = stacks ! from
  let t = stacks ! to
  let (f', t') = move n (f, t)
  let update = (insert from f') . (insert to t')
  put (update stacks)
  return ()

-- The CrateMover 9000
c9k 0 (from, to) = (from, to)
c9k n (x : from, to) = c9k (n - 1) (from, x : to)
c9k n ([], to) = undefined

-- Find the labels on the top crates for a set of stacks.
top_labels stacks = map head (elems stacks)

one = do
  (stacks, moves) <- input "input.txt"
  let procedure = traverse (apply_move c9k) moves
  let result = execState procedure stacks
  print (top_labels result)

-- The CrateMover 9001
c9k01 0 (from, to) = (from, to)
c9k01 n (x : from, to) =
  let (f, t) = c9k01 (n - 1) (from, to)
   in (f, x : t)
c9k01 n ([], to) = undefined

two = do
  (stacks, moves) <- input "input.txt"
  let procedure = traverse (apply_move c9k01) moves
  let result = execState procedure stacks
  print (top_labels result)
