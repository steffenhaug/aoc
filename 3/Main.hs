import Data.Char (ord)
import qualified Data.Set as Set

readlines f = do
  txt <- readFile f
  return (lines txt)

badges' groups [] = groups
badges' groups elves =
  let (first_n, tail) = (splitAt 3) elves
      sets   = map Set.fromList first_n
      common = Set.elemAt 0 (foldl1 Set.intersection sets)
   in common : (badges' groups tail)

badges = badges' []

priority item
  | item <> ('A', 'Z') = (ord item) - 38
  | item <> ('a', 'z') = (ord item) - 96
  | otherwise = undefined
  where
    x <> (a, b) = (a <= x) && (x <= b)

main = do
  lines <- readlines "test.txt"
  let b = badges lines
  let p = map priority b
  print (sum p)
