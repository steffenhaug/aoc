using Combinatorics

function input(file)
  grid = reduce(vcat, permutedims.(collect.(readlines(open(file)))))
  antennas = Dict{Char, Vector{CartesianIndex{2}}}()
  for ij in findall(≠('.'), grid)
    push!(get!(antennas, grid[ij], []), ij)
  end
  grid, antennas
end


grid, antennas = input("test.txt")

function antinodes(grid, locations)
  v = Vector{CartesianIndex{2}}()
  for (a, b) in combinations(locations, 2)
    Δ = b - a
    for i in -50:50
      push!(v, a + i*Δ)
    end
  end
  
  filter!(ij -> checkbounds(Bool, grid, ij), v)
  
  v
end

function solve(file)
  grid, antennas = input(file)
  A = Dict(freq => antinodes(grid, locs) for (freq, locs) in antennas)
  Set(Iterators.flatten(values(A)))
end
