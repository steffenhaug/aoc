using LinearAlgebra

input(file) = reduce(vcat, permutedims.(collect.(readlines(open(file)))))

function solve(file)
  board = input(file)
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

solve("test.txt")
