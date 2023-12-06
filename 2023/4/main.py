cards = [len(set.intersection(*[set(c.split()) for c in l.split(":")[1].split("|")])) for l in open(0).readlines()]
N = len(cards)

memo = [1] * N
for i in range(N):
    memo[i] += sum(memo[i - cards[N - i - 1] : i])
    
print(sum(memo))
for i, m in enumerate(memo):
    print(m, sum(memo[:i+1]))
    
