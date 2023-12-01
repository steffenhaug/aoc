open Aoc
open Aoc.IO

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
  let lines = readlines stdin in
  let rx    = Re.compiled "\\d|one|two|three|four|five|six|seven|eight|nine" in
  let digs  = List.map (Re.match_overlapping rx) lines in
  let nums  = List.map
      (fun digs ->
         let first = Vector.(get digs 0) in
         let last  = Vector.(get digs (length digs - 1)) in
         Int.of_string_exn (parse first ^ parse last))
      digs
  in
  let sum x = List.fold_right (+) x 0 in 
  Fmt.(printf "%i\n") (sum nums);
