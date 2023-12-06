open Aoc
open Aoc.IO

type interval =
  { start: int;
    len: int;
  }
[@@deriving show]

type map =
  { dst: int;
    src: int;
    len: int;
  }
[@@deriving show]
  
(* Input Parsing *)

let input =
  read stdin |>
  Str.split2 ~at:"\n\n" |>
  (fun (seeds, almanac) ->
     let seeds =
       Str.split seeds |>
       List.drop 1 |>
       List.map Re.int |>
       List.sublists_of_len 2 |>
       List.map (fun [start; len]  -> { start; len })
     in

     let almanac =
       Str.split ~at:"\n\n" almanac |>
       List.map
         (fun a ->
            Str.split ~at:"\n" a |>
            List.drop 1 |>
            List.map
              (fun m ->
                 let [dst; src; len] = Re.ints m in
                 {dst; src; len}))
     in
     
     seeds, almanac)
  
(* Part Two
   Strategy:
   To apply a set of mappings to a set of intervals,
   we maintain an input stack and an output stack.

   If (a section of) a range is mapped, we push the mapped range to the output stack.
   If (a section of) a range is unmapped, we push it back to the input stack to
   try another rule.
*)
  
let () =
  let seeds, almanac = input in
  Fmt.(printf "%a\n" (list pp_interval)) seeds;
