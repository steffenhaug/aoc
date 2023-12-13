open Aoc
open Aoc.IO

let input =
  let lines = readlines stdin in 
  lines |> List.map
    Str.(fun l ->
        let _, l = split2 ~at:":" l in
        let w, c = split2 ~at:"|" l in
        let w, c = split (trim w), split (trim c) in
        (* For every line, our input is |W ∩ C| *)
        List.length (List.inter ~eq:(=) w c))
    
let input = Array.of_list input
    
(* Part One *)
let p1 () = 
  let pts = Array.map (fun x -> 2**x / 2) input in
  let sum = Array.fold_right (+) pts 0 in
  Fmt.(pr "Part one: @{<Green>%i@}\n") sum

(* Part Two, Top down DP *)
let p2 () =
  let n = Array.length input in
  let memo = Array.make n 0 in

  let rec count (i: int) =
    match Array.get memo i with
    | 0 -> (* Nothing memoized *)
      let range = List.range (i + 1) (i + input.(i)) in
      let subproblems = List.map count range in
      let x = 1 + List.fold_right (+) subproblems 0 in
      Array.set memo i x;
      x
    | mem -> mem
  in

  let sum = List.fold_right (+) (List.map count List.(0--^n)) 0 in 
  
  Fmt.(pr "Part two:             @{<Green>%i@}\n") sum

    
(* Part Two, Bottom up DP *)
let p2' () =
  let n = Array.length input in
  let memo = Array.make n 1 in
  
  for i = 0 to n - 1 do
    let len = input.(n - i - 1) in
    let range = Array.sub memo (i - len) len in
    let x = 1 + Array.fold_right (+) range 0 in
    Array.set memo i x
  done;
  
  let sum = Array.fold_right (+) memo 0 in
  Fmt.(pr "Part two (bottom up): @{<Green>%i@}\n") sum

let input = Array.to_list input
    
(* Part Two, Top down DP as a fold *)
let p2'' () =
  let n = List.length input in
  let memo = Array.make n 0 in
  
  Fmt.(pr "%a\n" (List.pp ~pp_sep:comma int) input);

  let rec count rem (i: int) =
    match Array.get memo i with
    | 0 -> (* Nothing memoized *)
      (match rem with
       | [] -> 0
       | c :: cs ->
         (* range is i+1, ... i+c *)
         let range = List.range (i + 1) (i + c) in
         let copies = List.take c cs in
         let subproblems = count copies (i + 1) in
         let x = 1 + c in
         Array.set memo i x;
         x)
    | mem -> mem
  in

  let sum = count input 0 in
  
  Fmt.(pr "%a\n" (Array.pp ~pp_sep:comma int) memo);
  Fmt.(pr "Part two (fold):      @{<Green>%i@}\n") sum
    
  
let () =
  p1 ();
  p2 ();
  p2' ();
  p2'' ();
