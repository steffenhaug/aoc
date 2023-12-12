function getinput(f)
    springs = [(l[1], parse.(Int, split(l[2], ",")))
               for l in [split(l) for l in readlines(f)]]

    springs
end

s = getinput("test.txt")
display(s)
