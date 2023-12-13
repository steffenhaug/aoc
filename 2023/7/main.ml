open Aoc
open Aoc.IO

type ty =
  { best: int;
    next: int;
  }
[@@deriving show]

type hand =
  { t:   ty;
    h:   int list;
    bet: int;
  }
[@@deriving show]
  
let strength = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'T' -> 10
  | 'J' -> 0  (* Joker *)
  |  c  -> Char.code c - 48
           
let ty hand =
  let open CCMultiSet.Make(Int) in
  let counts = of_list hand in
  let jokers = count counts 0 in
  let counts = remove_all counts 0 in

  let (best, next) = fold counts (0, 0)
      (fun (m, s) count _ ->
         if      count > m then (count, m)
         else if count > s then (m, count)
         else                   (m, s)) in
  
  { best = best + jokers;
    next;
  }

let cmp h1 h2 =
  let rec fst_distinct l1 l2 =
    let a = List.hd l1 in
    let b = List.hd l2 in
    if a <> b then (a, b)
    else fst_distinct (List.tl l1) (List.tl l2)
  in
  
  if      h1.t.best <> h2.t.best then Int.compare h1.t.best h2.t.best
  else if h1.t.next <> h2.t.next then Int.compare h1.t.next h2.t.next
  else
    let d1, d2 = fst_distinct h1.h h2.h in
    Int.compare d1 d2
  
let () = 
  let input =
    readlines stdin |>
    List.map
      (fun line ->
         let (hand, bet) = Str.split2 line in
         let bet = Re.int bet in
         let h = List.map strength (Str.to_list hand) in
         let t = ty h in
         { t; h; bet }) in

  let n = List.length input in
  
  let sorted_bets = List.map (fun h -> h.bet) (List.sort cmp input) in

  let winnings = List.combine sorted_bets (List.range 1 n) |>
                 List.map (fun (bet, rank) -> bet * rank) |>
                 List.fold_left (+) 0 in

  Fmt.(pr "Part Two: @{<Green>%i@}\n") winnings
  
