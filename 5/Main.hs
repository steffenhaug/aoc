import Control.Monad.State
import Data.IntMap (IntMap, elems, insert, (!))
import qualified Data.IntMap as Map
import Input

type Move = (Int, Int, Int)

type Crane = Int -> ([Char], [Char]) -> ([Char], [Char])

type Stacks = IntMap [Char]

-- The CrateMover 9000
crane :: Crane
crane 0 (from, to) = (from, to)
crane n (x : from, to) = crane (n - 1) (from, x : to)
crane n ([], to) = undefined

-- Apply a CrateMover motion in the state monad.
apply_move :: Crane -> Move -> (State Stacks) ()
apply_move crane (n, from, to) = do
  stacks <- get
  let f = stacks ! from
  let t = stacks ! to
  let (f', t') = crane n (f, t)
  let update = (insert from f') . (insert to t')
  put (update stacks)
  return ()

-- Find the labels on the top crates for a set of stacks.
top_labels stacks = map head (elems stacks)

one = do
  (stacks, moves) <- input "input.txt"
  let procedure = traverse (apply_move crane) moves
  let result = execState procedure stacks
  print (top_labels result)

-- The CrateMover 9001
crane' :: Crane
crane' 0 (from, to) = (from, to)
crane' n (x : from, to) =
  let (f, t) = crane' (n - 1) (from, to)
   in (f, x : t)
crane' n ([], to) = undefined

two = do
  (stacks, moves) <- input "input.txt"
  let procedure = traverse (apply_move crane') moves
  let result = execState procedure stacks
  print (top_labels result)
