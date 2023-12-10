lines = readlines("in.txt")

r = r"(\d+) red"
g = r"(\d+) green"
b = r"(\d+) blue"

function p2()
    total = 0
    for l in lines
        red = maximum(parse(Int, g[1]) for g in eachmatch(r, l))
        green = maximum(parse(Int, g[1]) for g in eachmatch(g, l))
        blue = maximum(parse(Int, g[1]) for g in eachmatch(b, l))
        total += red * green * blue
    end
    total
end

println(@time p2())
