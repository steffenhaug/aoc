import Input

x ∈ (a, b) = (a <= x) && (x <= b)

i `contains` (a, b) = (a ∈ i) && (b ∈ i)

i `overlaps` (a, b) = (a ∈ i) || (b ∈ i)

count f xs = length (filter f xs)

contained (Assignments a b) = a `contains` b || b `contains` a

overlapping (Assignments a b) = a `overlaps` b || b `overlaps` a

one = do
  Right pairs <- input "input.txt"
  print (count contained pairs)

two = do
  Right pairs <- input "input.txt"
  print (count overlapping pairs)
