open Aoc
open Aoc.IO

let () = 
  let input = readlines stdin in
  List.iter (Fmt.pr "%s\n") input;
