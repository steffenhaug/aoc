const left, right = eachcol(vcat((parse.(Int, split(line))' for line in readlines(open("input.txt")))...))

sort!(left)
sort!(right)

# Part 1
part1(left, right) = @. sum(abs(left - right))
@time part1(left, right)

# Part 2
part2(left, right) = sum((id -> id * sum(right .== id)).(left))
@time part2(left, right)
