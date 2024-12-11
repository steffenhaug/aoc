using IterTools

function input(file)
  diskmap = parse.(Int, collect(readlines(file)[begin] * "0"))
  disk = Int64[]
  for (id, (n, n_free)) in enumerate(Iterators.partition(diskmap, 2))
    append!(disk, repeat([id - 1], n))
    append!(disk, repeat([-1], n_free))
  end
  disk
end

struct Segment
  id::Int64
  offset::Int64
  length::Int64
end

function segments(disk)
  [Segment(last(seg[begin]), first(seg[begin]), length(seg))
   for seg in groupby(last, enumerate(disk))]
end

freesegments(disk) = filter(seg -> seg.id == -1, segments(disk))

function render(disk)
  for d in disk
    if d == -1
      print('.')
    else
      print(d)
    end
  end
  println()
end

function defrag!(disk)
  N = maximum(disk)

  for id in N : -1 : 1
    segs = segments(disk)
    free = freesegments(disk)

    segment = segs[findlast(seg -> seg.id == id, segs)]
    free_candidate = findfirst(seg -> seg.length ≥ segment.length, free)

    if isnothing(free_candidate)
      continue
    end

    move_to = free[free_candidate]

    if move_to.offset < segment.offset
      disk[move_to.offset:move_to.offset+segment.length-1] .= disk[segment.offset:segment.offset+segment.length-1]
      disk[segment.offset:segment.offset+segment.length-1] .= -1
    end
  end
end

function checksum(disk)
  sum = 0
  for (position, fileid) in enumerate(disk)
    if fileid != -1
      sum += (position - 1) * fileid
    end
  end
  sum
end


function solve(file)
  disk = input(file)
  defrag!(disk)
  println(checksum(disk))
end

solve("test.txt")
