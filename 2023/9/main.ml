open Aoc
open Aoc.IO

let input = readlines (open_in "test.txt") |>
            List.map (fun l -> Re.ints l)

let () = Fmt.(pr "Part One: %a\n" (list (list int)) input);
    
