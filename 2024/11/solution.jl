input(file) = parse.(Int, split(readline(file)))

countdigits(N)::Int = 1 + floor(log10(N))

function blink(stone)
  if stone == 0
    (1,)
  elseif digits(stone) % 2 == 0
    divrem(stone, 10^(countdigits(stone) ÷ 2))
  else
    (2024 * stone,)
  end
end

function count(memo, stone, i)
  if haskey(memo, (stone, i))
    return memo[(stone, i)]
  end

  if i ≥ 1
    Σ = sum(count(memo, s, i - 1) for s in blink(stone))
  else
    Σ = 1
  end

  memo[(stone, i)] = Σ
  return Σ
end


function solve(stones, blinks)
  memo = Dict{Tuple{Int, Int}, Int}()
  sum(count(memo, stone, blinks) for stone in stones)
end
