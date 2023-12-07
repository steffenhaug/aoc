const strength = Dict('A' => 13, 'K' => 12, 'Q' => 11, 'T' => 10, 'J' => 0)
const input = [(hand = [ isdigit(card) ? parse(Int, card) : strength[card] for card in line[1]],
          bet  = parse(Int, line[2]))
        for line in [split(l) for l in readlines(stdin)]]


function type(hand)
    # Count the cards
    counts = Dict()
    for card in hand.hand
        counts[card] = 1 + get(counts, card, 0)
    end
    
    # Jokers count as the best card
    boost = get(counts, 0, 0)
    counts[0] = 0
    ordered = sort(collect(values(counts)), rev=true)
    ordered[1] += boost
    
    ordered
end

function cmp(h1, h2)
    t1 = type(h1)
    t2 = type(h2)

    if t1 == t2
        return h1.hand < h2.hand
    else
        return t1 < t2
    end
end


function p2()
    sort!(input, lt=cmp)

    tot = 0
    
    for (i, h) in enumerate(input)
        tot += i * h.bet
    end

    return tot
end

ans = @time p2()
println(ans)
