open Aoc
open Aoc.IO

type interval =
  { s: int;
    e: int;
  }

type map =
  { dst: int;
    s0: int;
    e0: int;
  }
  
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
       List.map (fun [s; l]  -> { s; e = s+l })
     in

     let almanac =
       Str.split ~at:"\n\n" almanac |>
       List.map
         (fun a ->
            Str.split ~at:"\n" a |>
            List.drop 1 |>
            List.map
              (fun m ->
                 let [dst; s0; l] = Re.ints m in
                 {dst; s0; e0 = s0 + l}))
     in
     
     seeds, almanac)

(* Part Two *)
  
(* The idea is, for each step, to maintain a stack of unmapped intervals.
   We iteratively pop an interval from the stack, search for an applicable
   projectcion rule, and put any leftover unmapped sub-intervals back on
   the stack.
*)

let project out seeds mapping =
  (* Take a seed from the stack. *)
  let {s = s1; e = e1} = List.hd seeds in
  
  (* Search for an applicable rule. *)
  let rec loop mapping =
    match mapping with
    | { dst; s0; e0} :: rem ->
      let s = max s1 s0 in
      let e = min e1 e0 in
      if s < e then
        (* Calculate the intersection. *)
        let inter = { s = (dst - s0) + s; e = (dst - s0) + e } in
        
        (* If there are unmapped seeds, they go back on the stack. *)
        match (s1 < s, e < e1) with
        | false, false -> inter :: out,                                       List.tl seeds
        | true,  false -> inter :: out, {s = s1; e = s}                    :: List.tl seeds
        | false,  true -> inter :: out,                    {s = e; e = e1} :: List.tl seeds
        | true,   true -> inter :: out, {s = s1; e = s} :: {s = e; e = e1} :: List.tl seeds
                                          
      else
        (* Empty intersection. *)
        loop rem
          
    (* All rules checked without match; project the seeds unmapped. *)
    | [] -> { s = s1; e = e1 } :: out, List.tl seeds
              
  in loop mapping
  
(* Apply one step of mapping to a complete interval stack. *)
let combine seeds mapping =
  let rec loop (out, seeds) =
    if List.is_empty seeds then
      out
    else
      loop (project out seeds mapping)
        
  in loop ([], seeds)

let () =
  let seeds, almanac = input in
  let locs = List.fold_left combine seeds almanac in
  let starts = List.map (fun i -> i.s) locs in
  let minimum = List.fold_left min (List.hd starts) (List.tl starts) in
  Fmt.(pr "Part Two: @{<Green>%i@}\n") minimum;
