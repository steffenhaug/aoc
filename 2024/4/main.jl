using LinearAlgebra
using IterTools

input(file) = reduce(vcat, permutedims.(collect.(readlines(open(file)))))

function part1(board)
  count = 0
  (M, N) = size(board)

  inbounds(i, j) = 1 ≤ i ≤ M && 1 ≤ j ≤ N

  for i in 1:M
    for j in 1:N
      for (Δi, Δj) in setdiff(product(-1:1, -1:1), ((0, 0),))
        idx(n) = (i + n * Δi, j + n * Δj)
        word = join(board[i, j] for (i, j) in idx.(0:3) if inbounds(i, j))
        if word == "XMAS"
          count += 1
        end
      end
    end
  end

  count
end

function part2(board)
  count = 0
  (M, N) = size(board)

  for i in 2:M-1
    for j in 2:N-1
      x = @view board[i-1:i+1, j-1:j+1]
      if sum(@. String(diag(rotl90((x,), 1:4))) == "MAS") == 2
        count += 1
      end
    end
  end

  count
end
