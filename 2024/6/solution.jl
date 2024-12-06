function input(file)
  grid = reduce(vcat, permutedims.(collect.(readlines(open(file)))))
  start = findfirst(==('^'), grid)
  obstacles = findall(==('#'), grid)
  size(grid), obstacles, start
end

const up = CartesianIndex(-1, 0)

turn(dir) = CartesianIndex(dir[2], -dir[1])

function search(bounds, obstacles, position, direction)
  inbounds(ij) = ij in CartesianIndices(bounds)
  clear(ij) = ij ∉ obstacles

  path = Set()

  while inbounds(position) && (position, direction) ∉ path
    push!(path, (position, direction))

    if clear(position + direction)
      position += direction
    else
      direction = turn(direction)
    end
  end

  visited = Set(pos for (pos, _) in path)
  looped = position in visited
  visited, looped
end

function solve(file)
  bounds, obstacles, start = input(file)
  path, _ = search(bounds, obstacles, start, up)
  println(length(path))
  sum(search(bounds, obstacles ∪ (pos,), start, up)[2] for pos in path)
end

solve("test.txt")
