import qualified Data.Set as Set

readlines f = do
  txt <- readFile f
  return (lines txt)

sop' i n xs =
  let candidate = take n xs
   in if length (Set.fromList candidate) == n
        then i + n
        else sop' (i + 1) n (drop 1 xs)

sop n = sop' 0 n

printall :: Show a => [a] -> IO ()
printall = mapM_ print

one = do
  lines <- readlines "input.txt"
  let positions = map (sop 4) lines
  printall positions

two = do
  lines <- readlines "input.txt"
  let positions = map (sop 14) lines
  printall positions
