i = read(stdin, String)
s, a ... = split(i, "\n\n") 
s = [parse(Int, s) for s in split(s)[2:end]]

const seeds = collect(zip(s[1:2:end], s[1:2:end] .+ s[2:2:end]))
const almanac = [[(dst=t[1], s0=t[2], e0=t[2]+t[3]) for t in [[parse(Int, digit) for digit in split(line)] for line in split(strip(map), "\n")[2:end]]] for map in a]
    
# Take a single seed, and apply the first applicable map.
function project!(out, seeds, map)
    (s1, e1) = pop!(seeds)    
    for (dst, s0, e0) in map
        s = max(s1, s0)
        e = min(e1, e0)
        if s < e
            push!(out, (dst - s0 + s, dst - s0 + e))
            if s1 < s
                push!(seeds, (s1, s))
            end
            if e < e1
                push!(seeds, (e, e1))
            end
            return
        end
    end
    push!(out, (s1, e1))
end
          
# Maintain a stack of input intervals, and iteratively project
# intervals into the output range until there are none left.
function map!(seeds, map)
    out = Tuple{Int64, Int64}[]
    while !isempty(seeds)
        project!(out, seeds, map)
    end
    return out
end

# Fold everything together
function p2!(seeds, almanac)
    for a in almanac
        seeds = map!(seeds, a)
    end
    
    return minimum(seeds)[1]
end

ans = @time p2!(seeds, almanac)
printstyled(ans, "\n", bold=true, color=:green)
