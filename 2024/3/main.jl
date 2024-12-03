const rx = r"(mul|do|don't)\((?:(\d+),(\d+))?\)"

function solve(file)
  sum = 0
  active = true

  for (opcode, imm1, imm2) in eachmatch(rx, read(file, String))
    if opcode == "don't"
      active = false
    elseif opcode == "do"
      active = true
    elseif opcode == "mul" && active
      x, y = parse.(Int64, (imm1, imm2))
      sum += x * y
    end
  end

  return sum
end

# julia> @time solve("input.txt")
#   0.000351 seconds (6.40 k allocations: 271.148 KiB)
