read_lines file = do
  txt <- readFile file
  return (lines txt)

-- Group lines separated by empty lines with a fold-esque operation.
group' [] lines = group' [[]] lines
group' groups@(g : gs) (l : ls) =
  case l of
    "" -> group' ([] : groups) ls
    line -> group' ((line : g) : gs) ls
group' groups [] = groups

group = group' []

-- Invariant: x < y < z.
max3' (x, y, z) [] = (x, y, z)
max3' (x, y, z) (t : ts)
  | z < t = max3' (y, z, t) ts
  | y < t = max3' (y, t, z) ts
  | x < t = max3' (t, y, z) ts
  | otherwise = max3' (x, y, z) ts

max3 = max3' (0, 0, 0)

main = do
  lines <- read_lines "input.txt"
  let read' = (read :: String -> Int)
  let sum_group = sum . (map read')
  -- Sum groups of lines.
  let calories = map sum_group (group lines)

  let (x, y, z) = max3' (0, 0, 0) calories
  print (x + y + z)
