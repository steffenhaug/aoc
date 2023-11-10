import Control.Monad.State
import Data.Map (Map, fromList, (!))
import qualified Data.Map as Map
import Input

-- This AoC solution uses monadic actions on a Zippered
-- Tree to represent the file system on the Elves' device.
-- Paper: Gérard Huet - Function Pearl; The Zipper.

data Zipper
  = Top
  | Node Fs Zipper
  deriving (Show)

data Cursor = Cursor Fs Zipper deriving (Show)

-- Move up one directory.
up :: Cursor -> Maybe Cursor
up (Cursor t z) =
  case z of
    Top -> Nothing
    Node (File _ _) _ -> Nothing
    Node (Dir pname siblings) up -> Just (Cursor parent up)
      where
        siblings' = Map.insert (filename t) t siblings
        parent = Dir pname siblings'

-- Move into the given subdirectory.
cd :: String -> Cursor -> Maybe Cursor
cd child (Cursor t z) =
  case t of
    File _ _ -> Nothing
    Dir name children -> Just (Cursor c z')
      where
        c = children ! child
        cs = Map.delete child children
        t' = Dir name cs
        z' = Node t' z

-- Iterate up to the root directory.
root :: Cursor -> Cursor
root cursor =
  case (up cursor) of
    Nothing -> cursor
    Just c -> root c

-- Extend a directory based on output from `ls`:
extend :: Cursor -> [Fs] -> Cursor
extend (Cursor t z) fs =
  case t of
    File _ _ -> undefined
    Dir name children -> Cursor t' z
      where
        t' = Dir name m'
        kvs = map (\f -> (filename f, f)) fs
        m' = Map.union (Map.fromList kvs) children

-- Execute a command on a filesystem cursor.
exec :: Cmd -> (State Cursor) ()
exec cmd = do
  cur <- get
  let Just cur' =
        ( case cmd of
            Cd ".." -> up cur
            Cd "/"  -> Just (root cur)
            Cd dir  -> cd dir cur
            Ls out  -> Just (extend cur out)
        )
  put cur'
  return ()

-- Recursively calculate the size of a filesystem object.
size :: Fs -> Int
size (File name size) = size
size (Dir name contents) = sum (map size (Map.elems contents))

-- Flatten the three into an array of every directory.
directories :: Fs -> [Fs]
directories (File name size) = []
directories d@(Dir name content) =
  let c' = Map.elems content
      subdirs = map directories c'
   in foldr (++) [d] subdirs

printall :: Show a => [a] -> IO ()
printall = mapM_ print

main = do
  log <- input "input.txt"

  -- Start with the cursor on the root node of an empty filesystem.
  let cursor = Cursor (Dir "/" Map.empty) Top

  -- Construct the tree with monadic actions as we explore the filesystem.
  let procedure = traverse exec log
  let result = execState procedure cursor
  let (Cursor fs Top) = root result

  -- Part One.
  let dirs = directories fs
  let sizes = map size dirs
  print $ sum (filter (<100000) sizes)

  -- Part Two.
  let threshold = 30000000 - (70000000 - size fs)
  let candidates = filter (>threshold) sizes
  print $ minimum candidates
