open Aoc
open Aoc.IO

let input =
  readlines stdin |>
  List.map
    (fun line ->
       let springs, nums = Str.split2 line in
       let springs = String.concat "?" (List.repeat 5 [springs]) in
       let nums = String.concat "," (List.repeat 5 [nums]) in
       let nums = Re.ints nums in
       let springs = String.to_list springs in
       springs, nums)

let count springs groups =
  (* Memoized auxillary function to handle guessing and backtracking. *)
  let memo = Hashtbl.create 256 in
  let rec aux springs gs =
    match Hashtbl.get memo (springs, gs) with
    | Some n -> n
    | None -> 
      let ans = match gs with
        (* We need to place the group `g`. *)
        | g :: gs' ->
          (match springs with
           (* If we run out of springs, we have no valid placement. *)
           | [] -> 0 
           (* If we are looking at an operational spring, keep looking. *)
           | '.' :: rem -> aux rem gs
           (* If we are looking at a damaged spring, try to place `g`. *)
           | '#' :: _ -> place g springs gs'
           (* If we are looking at an unknown spring, try both options. *)
           | '?' :: rem ->
             let a = place g springs gs' in
             let b = aux rem gs in
             a + b)
        (* When we run out of groups, we reached 1 valid placement
           only if there is no more broken springs that need a group.
        *)
        | [] -> if not (List.memq '#' springs) then 1 else 0
      in
      (* Update the memoization table. *)
      Hashtbl.replace memo (springs, gs) ans;
      ans

  and place g springs gs = 
    (* Try to place `g` damaged springs. *)

    let broken, rem = List.take_drop g springs in

    (* If there was <g springs left, we can't place g. *)
    let space = List.length broken = g in

    (* If the g next springs contain a known operational spring,
       we can't place g.
    *)
    let fit = not (List.memq '.' broken) in

    let can_place = space && fit in

    (* If we aren't on the end, we need to make sure g isn't
       immediately followed by a damaged spring.
    *)
    match rem with
    | [] -> if can_place then aux [] gs else 0
    | h :: t -> 
      let can_place = can_place && not Char.(h = '#') in
      if can_place then aux t gs else 0
  in

  aux springs groups

let () = 
  let cs = input |> List.map (fun (s, n) -> count s n) in
  let s = List.fold_right (+) cs 0 in
  Fmt.(printf "Part Two: @{<Green>%i@}\n" s)
