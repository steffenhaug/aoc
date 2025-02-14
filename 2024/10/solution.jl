input(file) = parse.(Int, reduce(vcat, permutedims.(collect.(readlines(file)))))

const north = CartesianIndex(-1,  0)
const south = CartesianIndex( 1,  0)
const east  = CartesianIndex( 0,  1)
const west  = CartesianIndex( 0, -1)

function search(grid, pos)
  if grid[pos] == 9
    return 1
  end

  inbounds(ij) = checkbounds(Bool, grid, ij)
  count = 0

  for dir in (north, south, east, west)
    next = pos + dir
    if inbounds(next) && grid[next] == 1 + grid[pos]
      count += search(grid, next)
    end
  end

  count
end

function solve(file)
  grid = input(file)
  zeros = findall(iszero, grid)
  sum(search(grid, start) for start in zeros)
end
