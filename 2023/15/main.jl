using DataStructures

getinput(f) = split(strip(read(f, String)), ',')

function hash(str)
    h = 0
    for c in str
        h += Int(c)
        h *= 17
        h %= 256
    end
    return h
end

function p1()
    sum(hash.(getinput("in.txt")))
end

function parseinstr(i)
    op = match(r"[-=]", i).match
    label, foc = split(i, op)
    op, label, foc
end

function p2()
    instructions = getinput("in.txt")
    boxes = [OrderedDict{String, Int}() for _ in 1:256]

    for instr in instructions
        op, label, foc = parseinstr(instr)
        box = boxes[1 + hash(label)]
        if op == "-"
            delete!(box, label)
        elseif op == "="
            box[label] = parse(Int, foc)
        end
    end

    sum(sum(i*j*f for (j, f) in enumerate(values(box)))
        for (i, box) in enumerate(boxes)
            if !isempty(box))
end

println("Part One: $(@time p1())")

# 0.001379 seconds (37.44 k allocations: 2.887 MiB)
println("Part Two: $(@time p2())")
