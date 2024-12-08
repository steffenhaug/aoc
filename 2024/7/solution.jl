function input(file)
  [(parse(Int64, r), parse.(Int64, split(cs)))
   for (r, cs) in split.(readlines(open(file)), ":")]
end

concat(x, y) = x * 10^(1 + floor(Int, log10(y))) + y

function possible(result, calvals)
  first, rest = calvals[1], calvals[begin+1:end]
  S₁ = Set(first)
  S₂ = Set{Int64}()
  for x in rest
    for s in S₁
      push!(S₂, s + x)
      push!(S₂, s * x)
      push!(S₂, concat(s, x))
    end
    filter!(≤(result), S₂)
    empty!(S₁)
    S₁, S₂ = S₂, S₁
  end
  result ∈ S₁
end

function solve(equations)
  count = 0
  for (result, calvals) in equations
    if possible(result, calvals)
      count += result
    end
  end
  count
end
