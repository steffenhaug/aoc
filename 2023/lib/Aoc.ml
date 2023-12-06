include Containers
    
(* My utility libraries. *)
module IO = AocIO
module Graph = AocGraph
module Z3 = AocZ3
module Sparse = AocSparse

module List = struct
  include CCList

  let range ?(step = 1) a b = CCList.range_by ~step:step a b
end

(* CC aliases. *)
module Queue = CCFQueue
module Deque = CCDeque
module Format = CCFormat
let _ = Format.set_color_default true

let sum xs = List.fold_right (+) xs 0
let ( ** ) b e = Int.pow b e
    
