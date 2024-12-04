using LinearAlgebra

input(file) = reduce(vcat, permutedims.(collect.(readlines(open(file)))))

function part1(board)
  count = 0
  (M, N) = size(board)

  inbounds(ij) = checkbounds(Bool, board, ij)

  for center in findall(==('X'), board)
    for direction in CartesianIndices((-1:1, -1:1))
      indices = [center + direction * offset for offset in 0:3]
      if all(inbounds(ij) for ij in indices)
        word = join(board[ij] for ij in indices)
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

board = input("test.txt")
println(part1(board))
println(part2(board))
