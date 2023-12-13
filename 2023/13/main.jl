function getinput(f)
    [reduce(vcat, permutedims.(collect.(split(strip(g), '\n'))))
              for g in split(read(f, String), "\n\n")]
end

@views function reflection(G)
    cols = size(G, 2) - 1
    for j = 1:cols
        L = G[:, j     : -1 : begin]
        R = G[:, j + 1 :  1 : end]
        n = min(size(L, 2), size(R, 2))

        if sum(L[:, begin:n] .!= R[:, begin:n]) == 1
            return j
        end
    end
    return 0
end

function p2(G)
    t = 0
    for g in G
        t += max(reflection(g), 100 * reflection(permutedims(g)))
    end
    return t
end

# 0.000472 seconds (7.89 k allocations: 738.727 KiB)
ans = @time p2(getinput("in.txt"))
println(ans)
