open Aoc
open Aoc.IO

type round = (string, int) Hashtbl.t

type game =
  { id:     int;
    rounds: round list;
  }

let ( >> ) g f = fun x -> f (g x)

(* Laying some serious pipe... *)

let parse =
  Str.split2 ~at:":" >>
  (fun (id, rounds) ->
      let id     = Re.int id in
      let rounds =
        rounds |>
        Str.split ~at:";" |>
        List.map
          (Str.trim >>
           Str.split ~at:"," >>
           List.map
             (Str.trim >>
              Str.split2 >>
              (fun (i, color) ->
                 color, Re.int i)) >>
          Hashtbl.of_list)
      in
      { id; rounds })

let red = 12
let green = 13
let blue = 14

let pow { id; rounds } =
  let rec loop rounds r' g' b' =
    match rounds with
    | []         -> r' * g' * b'
    | tab :: rem ->
      let r = Hashtbl.get_or ~default:1 tab "red" in
      let g = Hashtbl.get_or ~default:1 tab "green" in
      let b = Hashtbl.get_or ~default:1 tab "blue" in
      loop rem (max r r') (max g g') (max b b')
  in
  loop rounds 1 1 1

  

let () = 
  let input = readlines stdin in
  let input = List.map parse input in
  let powers = List.map pow input in
  let sum xs = List.fold_right ( + ) xs 0 in
  Fmt.(printf "%i\n") (sum powers);
