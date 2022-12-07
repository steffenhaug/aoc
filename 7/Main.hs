import Control.Monad.State
import Data.Map (Map, fromList, (!))
import qualified Data.Map as Map
import Input

-- This AoC solution uses monadic actions on a Zippered
-- Tree to represent the file system on the Elves' device.
-- Paper: Gérard Huet - Function Pearl; The Zipper.

data Tree
  = File String Int
  | Directory String (Map String Tree)
  deriving (Show)

data Zipper
  = Top
  | Node Tree Zipper
  deriving (Show)

data Cursor = Cursor Tree Zipper deriving (Show)

print_filesystem :: Tree -> IO ()
print_filesystem tree = psexpr' 0 tree
  where
    psexpr' indent tree =
      case tree of
        File name size -> putStrLn $ (replicate indent ' ') ++ name ++ " [" ++ (show size) ++ "]"
        Directory name children -> do
          putStrLn $ (replicate indent ' ') ++ name ++ " (dir)"
          mapM_ (psexpr' (indent + 2)) (Map.elems children)

-- Move the cursor up to the parent.
up :: Cursor -> Maybe Cursor
up (Cursor t z) =
  case z of
    Top -> Nothing
    Node (File _ _) _ -> Nothing
    Node (Directory pname siblings) up -> Just (Cursor parent up)
      where
        siblings' = Map.insert (name t) t siblings
        parent = Directory pname siblings'
        name (File name size) = name
        name (Directory name content) = name

-- Move the cursor down to a given child.
down :: String -> Cursor -> Maybe Cursor
down child (Cursor t z) =
  case t of
    File _ _ -> Nothing
    Directory name children -> Just (Cursor c z')
      where
        c = children ! child
        cs = Map.delete child children
        t' = Directory name cs
        z' = Node t' z

-- Iterate to move the cursor to the root.
root :: Cursor -> Cursor
root cursor =
  case (up cursor) of
    Nothing -> cursor
    Just c -> root c

-- Insert a new child under the cursor.
insert (Cursor t z) k v =
  case t of
    File _ _ -> undefined
    Directory name children -> Cursor t' z
      where
        t' = Directory name (Map.insert k v children)

-- Extend a directory based on output from `ls`:
extend (Cursor t z) fs =
  case t of
    File _ _ -> undefined
    Directory name children -> Cursor t' z
      where
        t' = Directory name m'
        fs' =
          Map.fromList
            ( map
                ( \x -> case x of
                    Dir n -> (n, Directory n Map.empty)
                    Fi n i -> (n, File n i)
                )
                fs
            )
        m' = Map.union children fs'

-- Execute a command on a filesystem cursor.
exec :: Cmd -> (State Cursor) ()
exec cmd = do
  cur <- get
  let Just cur' =
        ( case cmd of
            Cd "/" -> Just (root cur)
            Cd ".." -> up cur
            Cd dir -> (down dir) cur
            Ls out -> Just (extend cur out)
        )
  put cur'
  return ()

-- Recursively calculate the size of a filesystem object.
size :: Tree -> Int
size (File name size) = size
size (Directory name contents) = sum (map size (Map.elems contents))

-- Flatten the three into an array of every directory.
directories :: Tree -> [Tree]
directories (File name size) = []
directories d@(Directory name content) =
  let c' = Map.elems content
      subdirs = map directories c'
   in foldr (++) [d] subdirs

printall :: Show a => [a] -> IO ()
printall = mapM_ print

main = do
  log <- input "input.txt"

  -- Start with the cursor on the root node of an empty filesystem.
  let cursor = Cursor (Directory "/" Map.empty) Top

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
