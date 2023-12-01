open Aoc
open Aoc.IO

let match_overlapping rx input =
  let vec = Vector.create () in
  let rec scan pos =
    match Re.exec_opt ~pos:pos rx input with
    | None   -> ()
    | Some g ->
      let resume = 1 + Re.Group.start g 0 in
      let m      =     Re.Group.get   g 0 in
      Vector.push vec m;
      scan resume
  in scan 0;
  vec

let digits = match_overlapping (rx "\\d|one|two|three|four|five|six|seven|eight|nine")

let parse = function
  | "one"   -> "1"
  | "two"   -> "2"
  | "three" -> "3"
  | "four"  -> "4"
  | "five"  -> "5"
  | "six"   -> "6"
  | "seven" -> "7"
  | "eight" -> "8"
  | "nine"  -> "9"
  | dig     -> dig

let () = 
  let input = readlines stdin in
  let digs  = List.map digits input in
  let nums  = List.map
      (fun digs ->
         let first = Vector.(get digs 0) in
         let last  = Vector.(get digs (length digs - 1)) in
         Int.of_string_exn (parse first ^ parse last))
      digs
  in
  let sum x = List.fold_right (+) x 0 in 
  Fmt.(printf "%i\n") (sum nums);
