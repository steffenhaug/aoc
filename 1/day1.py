with open("input.txt") as f:
    txt = f.read()

txt = txt.split("\n\n")

calories = [[int(c) for c in g.strip().split("\n")]
                    for g in txt]

calsums = [sum(group) for group in calories]
print(max(calsums))
