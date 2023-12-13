open Aoc
open Aoc.IO

module Set = Set.Make(Int)

let rocks chs =
  let rec aux i = function
    |      [] -> []
    | ch::rem ->
      if Char.(ch = '#')
      then i :: aux (i + 1) rem
      else      aux (i + 1) rem
  in Set.of_list (aux 0 chs)

let input =
  read (open_in "test.txt") |>
  Str.split ~at:"\n\n" |>
  List.map (fun lines ->
      let lines = lines
                  |> Str.split ~at:"\n"
                  |> List.map Str.to_list in
      let v = List.map rocks lines in
      let h = List.map rocks (List.transpose lines) in
      v, h)

let mirror ?(trace = false) sets =
  if trace then Fmt.(pr "BEGIN TRACE\n");

  let rec aux pre sets =
    match sets with
    |      [ ] -> failwith "aux: empty list"
    |      [s] -> (pre, [s], [s])
    | s :: rem ->
      match aux (s :: pre) rem  with
      | pf,                [ ], st -> (pf, [ ], st)
      | (s :: pf),  (s' :: sf), st ->
        if Set.equal s s' then
          (pf, sf, s :: st)
        else
          (pf, s :: st, s :: st)
  in

  let p, s, t = aux [] sets in
  let n  = List.length sets in
  let n' = List.length t in
  let m  = List.length s in

  if      List.is_empty s then Some (n - n' / 2)
  else if List.is_empty p then
    if m = n then None
    else          Some ((n - m) / 2)
  else failwith "can't happen"
    

let () = 
  let opt = Fmt.(option ~none:(any "•") int) in
  input |> List.iteri (fun i (v, h) ->
      let mv = mirror v in
      let mh = mirror h in
      let x = 100 * Option.get_or ~default:0 mv in
      let x = Option.get_or ~default:x mh in
      Fmt.(pr "@[%i:\t%a\t%a \t-> %i@]@." i opt mv opt mh x));

  let ans = input |> List.map (fun  (v, h) ->
      let mv = mirror v in
      let mh = mirror h in
      let x = 100 * Option.get_or ~default:0 mv in
      let x = Option.get_or ~default:x mh in
      x)
  in
  let ans = List.fold_right (+) ans 0 in
  Fmt.(pr "@[Part One: %i@]@." ans);

