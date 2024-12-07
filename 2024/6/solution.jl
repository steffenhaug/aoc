const path_prealloc::Set{Tuple{CartesianIndex{2},CartesianIndex{2}}} =
  Set()
sizehint!(path_prealloc, 10_000)

function input(file)
  grid = reduce(vcat, permutedims.(collect.(readlines(open(file)))))
  start = findfirst(==('^'), grid)
  return grid, start
end

const north = CartesianIndex(-1, 0)

function search_p1(grid, position, direction)
  path = empty!(path_prealloc)
  turn(dir) = CartesianIndex(dir[2], -dir[1])
  inbounds(pos) = checkbounds(Bool, grid, pos)

  while inbounds(position) && (position, direction) ∉ path
    push!(path, (position, direction))
    if inbounds(position + direction) &&
       grid[position+direction] == '#'
      direction = turn(direction)
    else
      position += direction
    end
  end

  path = Set(first(step) for step in path)

  return path
end

function search_p2(grid, position, direction)
  path = empty!(path_prealloc)
  turn(dir) = CartesianIndex(dir[2], -dir[1])
  inbounds(pos) = checkbounds(Bool, grid, pos)

  while inbounds(position) && (position, direction) ∉ path
    push!(path, (position, direction))
    if inbounds(position + direction) &&
       grid[position+direction] == '#'
      direction = turn(direction)
    else
      position += direction
    end
  end

  looped = (position, direction) ∈ path

  return looped
end

function solve(grid, start)
  path = search_p1(grid, start, north)

  loops = 0
  for position in setdiff!(path, [start])
    grid[position] = '#'
    looped = search_p2(grid, start, north)
    grid[position] = '.'
    if looped
      loops += 1
    end
  end

  println(length(path))
  println(loops)
end

grid, start = input("input.txt")
println("Dry run for precompilation:")
@time solve(grid, start)

println("Benchmarking proper:")
@time solve(grid, start)
