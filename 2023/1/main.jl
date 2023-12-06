rx = r"(?=(\d|one|two|three|four|five|six|seven|eight|nine))"

lut = Dict(
    "one" =>   "1",
    "two" =>   "2",
    "three" => "3",
    "four" =>  "4",
    "five" =>  "5",
    "six" =>   "6",
    "seven" => "7",
    "eight" => "8",
    "nine" =>  "9",
)

lines = readlines(stdin)
digs = [[get(lut, m[1], m[1]) for m in eachmatch(rx, l)] for l in lines]
nums = [parse(Int, join(ds[[1, end]])) for ds in digs]
println(sum(nums))
