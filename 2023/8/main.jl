# %% Input Parsing
path, edges = split(read("test.txt", String), "\n\n")
edges = [split(line, " = ") for line in split(strip(edges), "\n")]

const P = [Symbol(d) for d in path]
const G = Dict([n[1] => NamedTuple{(:L, :R)}(split(n[2][2:end-1], ", ")) for n in edges])
const N = length(path)
const A = [n for n in keys(G) if endswith(n, "A")]
const Z = Set([n for n in keys(G) if endswith(n, "Z")])

# %% Traversal of the graph
function traverse(a, G, P)
    s = a
    i = 1
    
    while !(s in Z)
        d = P[mod(i, 1:N)]
        s = G[s][d]
        i += 1
    end

    i - 1
end

function cycles(A, G, P)
    C = [traverse(a, G, P) for a in A]
    lcm(C)
end

# %% Solve the problem
println(@time cycles(A, G, P))
