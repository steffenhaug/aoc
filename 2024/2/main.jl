using Combinatorics

reports = [parse.(Int64, line) for line in split.(readlines(open("input.txt")))]

function is_safe(report)
  delta = diff(report)
  all(1 .<= delta .<= 3) || all(-1 .>= delta .>= -3)
end

# 0.000129 seconds (7.19 k allocations: 263.797 KiB)
p1(reports) = sum(is_safe.(reports))

function is_safe_with_dampener(report)
  N = length(report)
  any(is_safe.(combinations(report, N - 1)))
end

# 0.001488 seconds (75.71 k allocations: 2.863 MiB)
p2(reports) = sum(is_safe_with_dampener.(reports))
