cards = [length(intersect([Set(split(c)) for c in split(split(l, ":")[2], "|")]...)) for l in readlines(stdin)]
N = length(cards)

memo = repeat([1], N)
for i = 1:N
    memo[i] += sum(memo[i - cards[(N + 1) - i] : i - 1])
end

println(sum(memo))
