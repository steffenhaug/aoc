using Combinatorics

function is_safe(report)
  delta = diff(report)
  all(@. 1 ≤ delta ≤ 3) || all(@. -1 ≥ delta ≥ -3)
end

function is_safe_with_dampener(report)
  any(is_safe.(combinations(report, length(report) - 1)))
end

function solve(file)
  reports = [parse.(Int64, line) for line in split.(readlines(open(file)))]

  p1 = sum(is_safe.(reports))
  p2 = sum(is_safe_with_dampener.(reports))

  println(p1)
  println(p2)
end

solve("test.txt")
