T, D = [parse(Int, join(split(l)[2:end])) for l in readlines(stdin)]

s1 = (T + isqrt(T^2 - 4*D)) ÷  2
s0 = (T - isqrt(T^2 - 4*D)) ÷  2
ds = s1 - s0

println(ds)
