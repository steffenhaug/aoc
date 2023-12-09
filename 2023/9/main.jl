const input = [[parse(Int,x) for x in split(line)] for line in readlines("in.txt")]

function extrapolate(v)
    N = length(v)
    M = zeros(Int, (N+1, N+1))
    M[1, 1:N] = v
    
    for i = 2 : N+1
        j = N + 1 - i
        M[i, 1:j] = M[i-1, 2:j+1] - M[i-1, 1:j]
    end

    for i = N : -1 : 1
        j = N + 2 - i
        M[i, j] = M[i, j-1] + M[i+1, j-1]
    end

    M[1,N + 1]
end

p1() = sum([extrapolate(v) for v in input])
p2() = sum([extrapolate(reverse(v)) for v in input])

println("Part One: ", @time p1())
println("Part Two: ", @time p2())
