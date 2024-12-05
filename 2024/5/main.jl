using Combinatorics

function input(file)
  fst, snd = split.(split(read(open(file), String), "\n\n"))
  order = Set([parse.(Int64, r) for r in split.(fst, "|")])
  pages = [parse.(Int64, l) for l in split.(snd, ",")]
  order, pages
end

middle(v) = v[length(v)÷2+1]

function solve(order, pages)
  compare(a, b) = [a, b] in order
  iscorrect(page) = issorted(page, lt=compare)
  correct(page) = sort(page, lt=compare)
  p1 = sum(middle(page) for page in pages if iscorrect(page))
  p2 = sum(middle(correct(page)) for page in pages if !iscorrect(page))
  p1, p2
end

function donny(order, pages)
  compare(a, b) = [a, b] in order
  p1 = p2 = 0
  for page in pages
    if issorted(page, lt=compare)
      p1 += middle(order)
    else
      sort!(page, lt=compare)
      p2 += middle(order)
    end
  end
  p1, p2
end

apple(order, pages) = sum(page[length(page)÷2+1] for page in pages if combinations(page, 2) ⊆ order)

order, pages = input("test.txt")
p1, p2 = solve(order, pages) # 0.000253 seconds (347 allocations: 31.359 KiB)
println(p1)
println(p2)
