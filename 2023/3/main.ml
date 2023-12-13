open Aoc
open Aoc.IO

let symbol = Re.compiled "[^\\d\\.\\n]"
let integer = Re.compiled "\\d+"
let gear = Re.compiled "\\*"
    
let sum xs = List.fold_right ( + ) xs 0
let product xs = List.fold_right ( * ) xs 1
    
let p1 input width =
  let numbers = Re.all integer input in
  
  let part_nos =
    numbers |>
    List.map
      (fun g ->
         (* Where is the number? *)
         let start = Re.start g in
         let stop = Re.stop g in

         (* "Columns" we are searching between *)
         let start = max (start - 1) 0 in
         let len = stop - start + 1 in

         (* Are there any symbols here? *)
         let box = String.sub input  start          len ^
                   String.sub input (start - width) len ^
                   String.sub input (start + width) len in
         let is_part = Re.execp symbol box in

         let part_no = Re.int (Re.str g) in
         
         is_part, part_no) |>
    List.filter fst |>
    List.map snd in

  Fmt.(pr "Part One: @{<green>%i@}\n") (sum part_nos)

                

let p2 input width =
  let gears = Re.all gear input in
  
  let surrounding_nos =
    gears |> List.map
      (fun g ->
         (* Where is the gear? *)
         let pos = Re.start g in

         (* The numbers in ±1 row. *)
         let start = pos - (pos mod width) - width in
         let len = 3 * width in
         let nums = Re.all ~pos:start ~len:len integer input in
         
         (* Discard numbers not next to the gear. *)
         nums |>
         List.filter
           (fun g ->
              let s = max (Re.start g mod width - 1) 0 in
              let e = Re.stop g mod width in
              let pos = pos mod width in 
              s <= pos && pos <= e) |>
         (* Extract the number from the regex group. *)
         List.map Re.str |>
         List.map Re.int) |>
    (* Keep gears with exactly 2 surrounding numbers. *)
    List.filter (fun adj -> List.length adj = 2) |>
    (* Calculate the gear ratio. *)
    List.map (fun adj -> product adj) in
  
  Fmt.(pr "Part Two: @{<green>%i@}\n") (sum surrounding_nos)

let () = 
  let input = read stdin in
  let width = 1 + Str.find ~sub:"\n" input in

  (* Padding the input removes some edge cases. *)
  let pad   = (Str.repeat "." (width - 1)) ^ "\n" in
  let input = pad ^ pad ^ input ^ pad ^ pad in
  
  p2 input width;
