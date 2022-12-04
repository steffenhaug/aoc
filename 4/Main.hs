import Input

x ∈ (a, b) = (a <= x) && (x <= b)

(a, b) ⊆ i = (a ∈ i) && (b ∈ i)

(a, b) ∩ i = (a ∈ i) || (b ∈ i)

count f xs = length (filter f xs)

contained (Assignments a b) = a ⊆ b || b ⊆ a

overlapping (Assignments a b) = a ∩ b || b ∩ a

one = do
  Right pairs <- input "input.txt"
  print (count contained pairs)

two = do
  Right pairs <- input "input.txt"
  print (count overlapping pairs)
