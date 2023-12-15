geti(f) = reduce(vcat, permutedims.(collect.(readlines(f))))

@views function load(G)
    m, n = size(G)
    t = 0
    for i = 1:m
        for j = 1:n
            if G[i, j] == 'O'
                t += m - i + 1
            end
        end
    end
    return t
end

@views function tilt!(v)
    (n,) = size(v)
    for i = 1:n
        if v[i] == 'O'
            for k = i:-1:1
                if k == 1 || v[k-1] != '.'
                    v[i] = '.'
                    v[k] = 'O'
                    break
                end
            end
        end
    end
end

@views function step!(G)
    tilt!.(eachcol(G))             # N
    tilt!.(eachrow(G))             # W
    tilt!.(eachcol(G[end:-1:1,:])) # S
    tilt!.(eachrow(G[:,end:-1:1])) # E
    return nothing
end

function p1(G)
    tilt!.(eachcol(G))
    load(G)
end

function p2(G)
    N = 1_000_000_000
    memo = Dict()
    for i = 0:N
        # When we find the same state twice, we have a cycle.
        if haskey(memo, G)
            cyclen = i - memo[G]
            # When the cycle length divides the remaining
            # no. of iterations, we are in the same state
            # we will be after N iterations.
            if mod(N - i, cyclen) == 0
                return load(G)
            end
        end
        memo[G] = i
        step!(G)
    end
end

# 0.062198 seconds (1.75 k allocations: 186.570 KiB)
ans = @time p2(geti("in.txt"))
println(ans)
