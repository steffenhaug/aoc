import time
i = open(0).read()
seeds, *almanac = i.split("\n\n")
seeds = [int(s) for s in seeds.split()[1:]]
seeds = list(zip(seeds[::2], seeds[1::2]))
almanac = [[[int(y) for y in x.split()]
        for x in a.strip().split("\n")[1:]]
        for a in almanac]

t0 = time.perf_counter_ns()

for a in almanac:
    out = []
    
    # We need to re-queue the "remainder" of intervals with no match.
    for s, l in seeds:
        matches = False
        
        # Test all the rules.
        for D, S, L in a:
            start = max(s, S)
            stop  = min(s+l, S+L)
            if start < stop:
                matches = True
                intersection = (D + start - S, stop - start)
                out.append(intersection)
                
                if s < start:
                    seeds.append((s, S - s))
                    
                if stop < s + l:
                    seeds.append((stop, s+l-S-L))

                break
            
        if not matches:
            out.append((s, l))
            
    seeds = out

t1 = time.perf_counter_ns()
delta = t1 - t0   
    
print(min(seeds, key=lambda t:t[0])[0])
print(f"Δt={delta}ns")
