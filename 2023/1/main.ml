open Aoc
open Containers
open Printf

let cart l l' =
  let q = CCDeque.create () in
  List.iter (fun x ->
      List.iter (fun x' ->
          CCDeque.push_back q (x, x'))
        l') l;
  CCDeque.to_list q

let () = 
  let open Aoc.Z3.DSL in
  let solver = Z3.Solver.solver () in

  (* r1, ..., r8 and c1, ..., c8 *)
  let rs = Z3.ints "r" (List.range 1 8) in
  let cs = Z3.ints "c" (List.range 1 8) in

  (* On a diagonal, either the sum or the diff. of row/col is constant. *)
  let d  = List.map2 (+) rs cs in
  let d' = List.map2 (-) rs cs in

  (* Coordinates are in { 1, ..., 8 }. *)
  let r = (Z3.Set.range 1 8) in
  Z3.Solver.add solver (List.map (Z3.Set.mem r) rs);
  Z3.Solver.add solver (List.map (Z3.Set.mem r) cs);

  (* Orthogonal and diagonal positions are distinct. *)
  Z3.Solver.add solver
    [ Z3.distinct rs;
      Z3.distinct cs;
      Z3.distinct d;
      Z3.distinct d';
    ];

  let st = Z3.Solver.check solver in

  (* Display the solution. *)

  match st with
  | Unknown | Unsat -> ()
  | Sat { model }   ->
    let queens = List.map
      (fun i ->
         let r = Hashtbl.find model (sprintf "r%i" i) in
         let c = Hashtbl.find model (sprintf "c%i" i) in
         (r, c))
        List.(1--8)
    in

    let queens = Hashtbl.of_seq
        (Seq.zip
           (Seq.of_list queens)
           (Seq.repeat ()))
    in

    List.iter
      (fun (r, c) ->
         printf "%s%s"
           (if Hashtbl.mem queens (r, c) then "*" else "·")
           (if Int.(c = 8) then "\n" else " "))
      (cart List.(1--8) List.(1--8))
