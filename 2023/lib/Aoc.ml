include Containers
    
(* My utility libraries. *)
module IO = AocIO
module Graph = AocGraph
module Z3 = AocZ3
module Sparse = AocSparse

module List = struct
  include CCList

  let range ?(step = 1) a b = CCList.range_by ~step:step a b

  (* https://www2.lib.uchicago.edu/keith/ocaml-class/utils.html *)
  let transpose ll =
    let rec transpose' acc = function
      | [] -> acc
      | []::_ -> acc
      | m -> transpose' ((List.map List.hd m)::acc) (List.map List.tl m)
    in
    List.rev (transpose' [] ll)

end

(* CC aliases. *)
module Queue = CCFQueue
module Deque = CCDeque
module Format = CCFormat
let _ = Format.set_color_default true

let sum xs = List.fold_right (+) xs 0
let ( ** ) b e = Int.pow b e
    
