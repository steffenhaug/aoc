import Input

x ∈ (a, b) = (a <= x) && (x <= b)

(a, b) ⊆ i = (a ∈ i) && (b ∈ i)

(a, b) ∩ (c, d) = a ∈ (c, d) || b ∈ (c, d) || c ∈ (a, b) || d ∈ (a, b)

count f xs = length (filter f xs)

contained (Assignments a b) = a ⊆ b || b ⊆ a

overlapping (Assignments a b) = a ∩ b

one = do
  Right pairs <- input "input.txt"
  print (count contained pairs)

two = do
  Right pairs <- input "input.txt"
  print (count overlapping pairs)
