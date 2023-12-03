import re
import time

i = open(0).read()
w = 1 + i.find("\n")

# Padding the input removes edge cases!
pad = "." * (w - 1) + "\n"
i = 2*pad + i + 2*pad
print(i)


integer = re.compile(r"\d+")
part = re.compile(r"[^\d\.\n]")
gear = re.compile(r"\*")


# Part One
s, numbers = 0, integer.finditer(i)
for m in numbers:
    # Where is the number?
    start = m.start() - 1
    end = m.end() + 1
    
    # The "box" surrounding the number.
    box = i[start-w:end-w] + i[start:end] + i[start+w:end+w]
    if part.search(box):
        s += int(m[0])

print(s)
    
    
# Part Two
t0 = time.perf_counter_ns()

ratios, gears = 0, gear.finditer(i)
for m in gears:
    # Where is the gear?
    pos = m.start()

    # Get the numbers in ±1 row.
    start = pos - (pos % w) - w
    end = start + 3 * w
    nums = integer.finditer(i, pos=start, endpos=end)

    p = 1
    j = 0
    for n in nums:
        s = max(n.start() % w - 1, 0)
        e = n.end() % w 
        pos = pos % w
        if s <= pos <= e:
            p *= int(n[0])
            j += 1
            
    if j == 2:
        ratios += p

t1 = time.perf_counter_ns()
delta = t1 - t0
        
print(ratios)
print(f"Δt={delta}ns")
