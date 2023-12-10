open Aoc
open Aoc.IO

type dir = N | S | E | W [@@deriving show, eq]

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
    readlines stdin |>
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

let winding (i, j) loop =
  (* Find all the crossing right of ij *)
  if Hashtbl.mem loop (i, j) then None
  else
    let crossings =
      Hashtbl.keys_list loop |>
      List.filter (fun (i', j') -> i' = i && j' > j) |>
      List.sort (fun (_, j) (_, j') -> Int.compare j j')
    in

    Fmt.(printf "Crossings: %a\n" (list (pair int int)) crossings);

    let goes dir ij =
      let _, dir' = Hashtbl.find loop ij in
      equal_dir dir' dir
    in

    let comes dir ij =
      let dir',_ = Hashtbl.find loop ij in
      equal_dir dir' dir
    in

    (* crossings arent fucking sorted :v)))) *)

    let rec deltas = function
      | [] -> []
      | ij :: rem ->
        Fmt.(printf "%a\n" (list (pair int int)) (ij::rem));
        match Hashtbl.find loop ij with
        | N, S ->  1 :: deltas rem
        | S, N -> -1 :: deltas rem

        | S, E ->
          let ij' :: rem' = List.drop_while (goes E) rem in
          let link' = Hashtbl.find loop ij' in
          (match link' with
           | _, N -> -1 :: deltas rem'
           | _, S ->  0 :: deltas rem')

        | N, E ->
          let ij' :: rem' = List.drop_while (goes E) rem in
          let link' = Hashtbl.find loop ij' in
          (match link' with
           | _, N -> 0 :: deltas rem'
           | _, S -> 1 :: deltas rem')

        | E, N ->
          let ij' :: rem' = List.drop_while (comes W) rem in
          let link' = Hashtbl.find loop ij' in
          (match link' with
           | N, W ->  0 :: deltas rem'
           | S, W -> -1 :: deltas rem')

        | E, S ->
          let ij' :: rem' = List.drop_while (comes W) rem in
          let link' = Hashtbl.find loop ij' in
          (match link' with
           | N, W -> 1 :: deltas rem'
           | S, W -> 0 :: deltas rem')

        | _    ->       deltas rem

    in

    Some (List.fold_right (+) (deltas crossings) 0)


let () = 
  let loop = Hashtbl.create 1024 in
  Hashtbl.replace loop init (initfrom, initdir);
  traverse init initfrom loop;

  let x = (4, 9) in
  let w = winding x loop in

  Fmt.(printf "Start: %a\n" (pair int int)) start;
  Fmt.(printf "Loop: %i\n") (Hashtbl.length loop);
  Fmt.(printf "W(%i, %i) = %a\n" (fst x) (snd x) (opt int) w);

  for i = 0 to 20 do
    for j = 0 to 20 do
      match winding (i, j) loop with
      | None   -> print_string "@"
      | Some 0 -> print_string " "
      | Some 1 -> print_string "·"
      | Some c -> if c > 0 then print_int c
            else print_string "-"
    done;
    print_newline ()
  done
