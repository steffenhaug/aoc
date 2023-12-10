# %% Input parsing
function getinput(f)
    lines = readlines(f)
    grid = reduce(vcat, permutedims.(collect.(lines)))

    # Find the start
    m, n = size(grid)
    start = nothing

    for i in 1:m
        for j in 1:n
            if grid[i,j] == 'S'
                start = (i, j)

                if grid[i-1, j] in "F7|" && grid[i+1,j] in "|LJ"; grid[i,j] = '|'; end
                if grid[i, j-1] in "FL-" && grid[i,j+1] in "-7J"; grid[i,j] = '-'; end
                if grid[i+1, j] in "|LJ" && grid[i,j+1] in "-7J"; grid[i,j] = 'F'; end
                if grid[i, j-1] in "FL-" && grid[i+1,j] in "|LJ"; grid[i,j] = '7'; end
                if grid[i, j-1] in "FL-" && grid[i-1,j] in "|F7"; grid[i,j] = 'J'; end
                if grid[i-1, j] in "F7|" && grid[i,j+1] in "-7J"; grid[i,j] = 'L'; end
            end
        end
    end

    grid, start
end

const grid, start = getinput("in.txt")
const M, N = size(grid)

# %% Find the loop

# It doesn't matter if we do BFS or DFS or anything in between.
# We still visit every node in the loop once. This is a dodgy
# variant where I use a Set as a "queue".

function findloop()
    loop = Set()
    frontier = Set([start])
    while length(frontier) > 0
        i, j = pop!(frontier)
        push!(loop, (i, j))
        if grid[i, j] == '|'; union!(frontier, setdiff(Set([(i-1,j), (i+1, j)]), loop)) end
        if grid[i, j] == '-'; union!(frontier, setdiff(Set([(i,j-1), (i, j+1)]), loop)) end
        if grid[i, j] == 'F'; union!(frontier, setdiff(Set([(i+1,j), (i, j+1)]), loop)) end
        if grid[i, j] == 'L'; union!(frontier, setdiff(Set([(i-1,j), (i, j+1)]), loop)) end
        if grid[i, j] == '7'; union!(frontier, setdiff(Set([(i,j-1), (i+1, j)]), loop)) end
        if grid[i, j] == 'J'; union!(frontier, setdiff(Set([(i,j-1), (i-1, j)]), loop)) end
    end

    loop
end

# 0.004596 seconds (172.77 k allocations: 18.185 MiB)
# The bad performance is due to allocating a billion temporary sets to manage the "queue"
loop = @time findloop();
length(loop)

# %% Winding Number
# The winding number at a point with respect to a curve can be used to
# find the answer. Since the pipes form a planar graph, we have a slightly
# easier edge case where the orientation of the curve at a point doesn't
# matter; we can simply count the number of times a ray eminating from a
# point crosses the loop.

function W((i, j))
    # Points on the loop has W = 0
    if (i, j) in loop
        return 0
    end

    # Count intersections
    ray = grid[i,j+1:end]
    W = 0
    k = 1
    while k < length(ray)
        if (i, j + k) in loop
            #  A | is a simple intersection
            if ray[k] == '|'
                W += 1
            end

            # If we find the start of something like F---J, we scan ahead
            # for the matching 7/J by just skipping over '-'
            if ray[k] in "LF"
                start = ray[k]
                k += 1
                while ray[k] == '-'
                    k += 1
                end
                stop = ray[k]

                if start == 'L' && stop == '7'
                    W += 1
                end

                if start == 'F' && stop == 'J'
                    W += 1
                end
            end
        end
        
        k += 1
    end

    mod(W, 2)
end

function inside()
    S = Set()

    for i in 1:M
        for j in 1:N
            if W((i, j)) == 1
                push!(S, (i, j))
            end
        end
    end

    S
end

I = @time inside();

# %% Pretty print grid
boxdrawing = Dict(
    '.' => ' ' ,
    '|' => '║',
    '-' => '═',
    'F' => '╔',
    '7' => '╗',
    'J' => '╝',
    'L' => '╚'
)

function showgrid()
    for i in 1:M
        for j in 1:N
            if (i, j) in loop
                print(boxdrawing[grid[i,j]])
            elseif (i, j) in I
                print("I")
            else
                print(" ")
            end
        end
        print('\n')
    end
end

showgrid()
println("Answer: ", length(I))
