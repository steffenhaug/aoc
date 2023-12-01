import re

rx = re.compile("\\d|one|two|three|four|five|six|seven|eight|nine")

lut = {
    "one":   "1",
    "two":   "2",
    "three": "3",
    "four":  "4",
    "five":  "5",
    "six":   "6",
    "seven": "7",
    "eight": "8",
    "nine":  "9",
}

def overlapping(rx, line):
    matches = []
    pos = 0
    while g := rx.search(line, pos):
        pos = 1 + g.start()
        matches.append(g[0])
    return matches

lines = [overlapping(rx, line) for line in open(0).readlines()]
lines = [[lut.get(dig, dig) for dig in l] for l in lines]
nums  = [int(line[0] + line[-1]) for line in lines]
print(sum(nums))
