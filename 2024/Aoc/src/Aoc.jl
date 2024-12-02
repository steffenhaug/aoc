module Aoc

function inbound(board, (i, j))
    (M, N) = size(board)
    return i > 0 && j > 0 && i <= M && j <= N
end

function adj((i, j))
    Set([(i + 1, j),
         (i - 1, j),
         (i, j + 1),
         (i, j - 1)])
end

function flood(board, start)
    stack = [start]
    fill = Set([start])

    candidate(neighbor) = neighbor ∉ fill && inbound(board, neighbor)

    while !isempty(stack)
        (i, j) = pop!(stack)
        if board[i, j] == board[start...]
            push!(fill, (i, j))
            frontier = filter(candidate, adj((i, j)))
            append!(stack, frontier)
        end
    end

    return fill
end

end # module Aoc
