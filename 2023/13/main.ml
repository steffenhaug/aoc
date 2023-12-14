open Aoc
open Aoc.IO

module Set = Set.Make(Int)

(* Input parsing *)

let rocks chs =
  let rec aux i = function
    |      [] -> []
    | ch::rem ->
      if Char.(ch = '#')
      then i :: aux (i + 1) rem
      else      aux (i + 1) rem
  in Set.of_list (aux 0 chs)

let input =
  read (open_in "in.txt") |>
  Str.split ~at:"\n\n" |>
  List.map (fun lines ->
      let lines = lines
                  |> Str.split ~at:"\n"
                  |> List.map Str.to_list in
      let v = List.map rocks lines in
      let h = List.map rocks (List.transpose lines) in
      v, h)

(* Part Two *)

let smudgecount ls rs =
  List.(zipwith Set.symdiff ls rs
        |> map Set.cardinality
        |> sum)

let reflection sets =
  (* Auxillary function that traverses the list with a Zipper. *)
  let rec aux i zip =
    match zip with
    |  _,  []              -> 0
    | [], (r :: rem)       -> aux (i + 1) ([r], rem)
    | ls, (r :: rem as rs) ->
      if smudgecount ls rs = 1
      then i
      else aux (i + 1) (r :: ls, rem) in

  aux 0 ([], sets)

let () = 
  let ans =
    List.(input
          |> map (fun (v, h) ->
              let rv = reflection v in
              let rh = reflection h in
              max (100 * rv) rh)
          |> sum) in

  Fmt.(pr "@[Part One: %i@]@." ans);

