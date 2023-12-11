function getinput(f)
    input = reduce(vcat, permutedims.(collect.(readlines(f))))
    m, n = size(input)

    R = Set([i for (i, r) in enumerate(eachrow(input)) if all(r .== '.')])
    C = Set([i for (i, c) in enumerate(eachcol(input)) if all(c .== '.')])

    G = Set{Vector{Int}}()
    for i = 1:m
        for j = 1:n
            if input[i, j] == '#'
                Δi = 999_999 * length(R ∩ (1:i))
                Δj = 999_999 * length(C ∩ (1:j))
                push!(G, [i + Δi, j + Δj])
            end
        end
    end

    G
end

galaxies = @time getinput("in.txt");

# The cartesian product contains too many pairs, however
#   1. The (G, G) pairs have distance zero for all G
#   2. The (G, G') pairs are in there exactly twice for all G ≠ G'
# so in the end, we can just divide the sum of distances
# in the cartesian product by 2.

function p2()
    cart = Iterators.product(galaxies, galaxies)
    d(v, w) = sum(abs.(v - w)) # l¹ norm
    s = sum(d(G, G´) for (G, G´) in cart)
    s ÷ 2
end

println("Part Two: $(@time p2())")

# %% Print sparse matrix
M = maximum(g[1] for g in galaxies)
N = maximum(g[2] for g in galaxies)
for i = 1:M
    for j = 1:N
        if [i, j] in galaxies
            print('@')
        else
            print('•')
        end
    end
    print('\n')
end
