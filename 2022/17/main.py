size = {
    0: (4, 1),
    1: (3, 3),
    2: (3, 3),
    3: (1, 4),
    4: (2, 2)
}


def rock(x, y, r):
    if   r == 0: return { (x, y), (x+1, y), (x+2, y), (x+3, y) }
    elif r == 1: return { (x+1, y), (x+1, y+1), (x, y+1), (x+2,y+1), (x+1,y+2) }
    elif r == 2: return { (x,y),   (x+1, y), (x+2, y), (x+2,y+1), (x+2, y+2) }
    elif r == 3: return { (x, y), (x, y+1), (x, y+2), (x, y+3) }
    elif r == 4: return { (x, y), (x+1, y), (x+1, y+1), (x, y+1) }


def collides(x, y, R):
    if x              < 0: return True
    if y              < 0: return True
    if x + size[R][0] > 7: return True
    else:                  return rock(x, y, R) & tower


def print_state(rock, tower, height):
    for y in range(height-1,-1,-1):
        for x in range(7):
            if   (x, y) in rock:  print("@", end="")
            elif (x, y) in tower: print("#", end="")
            else:                 print("⋅", end="")
        print(f" {y}")

        
jets  = open(0).read().strip()
J     = len(jets)
tower = set()

top = 0
j   = -1
R   = 0

N    = 1_000_000_000_000
memo = dict()

for i in range(N):
    # Cycle detection: 
    if (R, j) in memo:
        then, t  = memo[R,j]
        # To remove an edge case, we specifically look for
        # a cycle that divides the remaining subproblem.
        div, mod = divmod(N - i, i - then)
        if mod == 0:
            top = div * (top - t) + top
            break

    memo[R,j] = (i, top)

    R, x, y = i % 5, 2, 3 + top

    while not collides(x, y, R):
        jet = jets[j := (j + 1) % J]
        if jet == "<" and not collides(x - 1, y, R): x -= 1
        if jet == ">" and not collides(x + 1, y, R): x += 1
        y -= 1

    tower |= rock(x, 1 + y, R)
    top    = max(top, 1 + y + size[R][1])

print(top)
