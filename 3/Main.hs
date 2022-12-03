import Data.Char (ord)
import qualified Data.Set as Set

readlines f = do
  txt <- readFile f
  return (lines txt)

group' n gs [] = gs
group' n gs es =
  let (g, e) = (splitAt n) es
   in group' n (g : gs) e

group3 = group' 3 []

priority item
  | item <= 'Z' = (ord item) - 38
  | item <= 'z' = (ord item) - 96
  | otherwise = 0

main = do
  lines <- readlines "input.txt"
  let groups = map (map Set.fromList) (group3 lines)
  let badges = map (foldl1 Set.intersection) groups
  let priorities = map (priority . Set.elemAt 0) badges
  print (sum priorities)
