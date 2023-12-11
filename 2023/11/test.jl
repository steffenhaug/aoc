using Combinatorics
using StaticArrays

function getinput(f)
    input = reduce(vcat, permutedims.(collect.(readlines(f))))
    m, n = size(input)

    R = Set([i for (i, r) in enumerate(eachrow(input)) if all(r .== '.')])
    C = Set([i for (i, c) in enumerate(eachcol(input)) if all(c .== '.')])

    G = Vector{SVector{2, Int}}()

    for i = 1:m
        for j = 1:n
            if input[i, j] == '#'
                Δi = 999_999 * length(R ∩ (1:i))
                Δj = 999_999 * length(C ∩ (1:j))
                push!(G, (i + Δi, j + Δj))
            end
        end
    end

    G
end

const galaxies = @time getinput("in.txt");

function p2(G)
    d(v, w) = sum(abs.(v - w)) # l¹ norm
    sum(d(g₁, g₂) for (g₁, g₂) in combinations(G, 2))
end

println("Part Two: $(@time p2(galaxies))")
