open Aoc
open Aoc.IO

type dir = N | S | E | W [@@deriving show { with_path = false } , eq]

type map = (int * int, char) Hashtbl.t [@@deriving show]

let north (i, j) = (i - 1, j)
let south (i, j) = (i + 1, j)
let east (i, j) = (i, j + 1)
let west (i, j) = (i, j - 1)


(* Parse the map *)

let (start: int * int),
    (input: map) =

  let start = ref (-1, -1) in

  let merge tab1 tab2 =
    Hashtbl.fold (fun key elt () -> Hashtbl.replace tab1 key elt) tab2 ();
    tab1
  in

  let input =
    readlines (open_in "test.txt") |>
    List.mapi
      (fun i line ->
         String.to_list line |>
         List.mapi
           (fun j (c: char) ->
              if Char.(c = 'S') then
                start := (i, j)
              else ();
              ((i, j), c)) |>
         Hashtbl.of_list) |>
    List.fold_left
      merge
      (Hashtbl.create 4096) in

  !start, input


(* Find the first step (kinda dodgy) *)

let init, initfrom, initdir =
  if String.contains "-7J" (Hashtbl.find input (east start)) then
    (east start), W, E
  else if String.contains "-LF" (Hashtbl.find input (west start)) then
    (west start), E, W
  else if String.contains "|F7" (Hashtbl.find input (north start)) then
    (north start), S, N
  else
    (south start), N, S


let rec traverse ij (from: dir) loop =
  let pipe = Hashtbl.find input ij in

  let go dir = 
    Hashtbl.replace loop ij (from, dir);
    match dir with
    | N -> traverse (north ij) S loop
    | S -> traverse (south ij) N loop
    | E -> traverse (east  ij) W loop
    | W -> traverse (west  ij) E loop
  in

  match (from, pipe) with
  | _, 'S' -> Hashtbl.replace loop start (from, initdir)
  | N, 'L' -> go E
  | N, '|' -> go S
  | N, 'J' -> go W
  | W, 'J' -> go N
  | W, '-' -> go E
  | W, '7' -> go S
  | S, 'F' -> go E
  | S, '7' -> go W
  | S, '|' -> go N
  | E, 'L' -> go N
  | E, '-' -> go W
  | E, 'F' -> go S

let () = 
  let loop = Hashtbl.create 1024 in
  Hashtbl.replace loop init (initfrom, initdir);
  traverse init initfrom loop;
