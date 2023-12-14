include Containers
    
(* My utility libraries. *)
module IO = AocIO
module Graph = AocGraph
module Z3 = AocZ3
module Sparse = AocSparse

module Set = struct
module Make (Ord : Set.OrderedType) = struct
  include Set.Make(Ord)
  let symdiff s s' = Set.(union (diff s s') (diff s' s))
  let cardinality = cardinal
end
end


module List = struct
  include CCList

  let fold = fold_left

  let sum  = fold ( + ) 0

  let prod = fold ( * ) 1

  let all = fold ( && ) true


  let range ?(step = 1) a b = CCList.range_by ~step:step a b

  (* https://www2.lib.uchicago.edu/keith/ocaml-class/utils.html *)
  let transpose ll =
    let rec transpose' acc = function
      | [] -> acc
      | []::_ -> acc
      | m -> transpose' ((List.map List.hd m)::acc) (List.map List.tl m)
    in
    List.rev (transpose' [] ll)


  let rec zipwith fn xs ys =
    match xs, ys with
    | x :: xs, y :: ys -> (fn x y) :: (zipwith fn xs ys)
    | _ -> []

  let rec zip xs ys = 
    match xs, ys with
    | x :: xs, y :: ys -> (x, y) :: (zip xs ys)
    | _ -> []

end

(* CC aliases. *)
module Queue = CCFQueue
module Deque = CCDeque
module Format = CCFormat
let _ = Format.set_color_default true

let ( ** ) b e = Int.pow b e
    
