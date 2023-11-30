open Containers

(* Wrap the real module in case we need to circumvent the nice API. *)
module Rawdog = Z3

(** A global Z3 context *)
module Ctx = struct
  type t = Z3.context
  let global = Z3.mk_context []
end

module Int = struct
  let sort = Z3.Arithmetic.Integer.mk_sort Ctx.global

  (** Create an integer numeral. *)
  let num k = Z3.Arithmetic.Integer.mk_numeral_i Ctx.global k
end

module Bool = struct
  let sort = Z3.Boolean.mk_sort Ctx.global
end

module Set = struct
  let sort = Z3.Set.mk_sort Ctx.global Int.sort

  (** The empty set (of integers) *)
  let empty = Z3.Set.mk_empty Ctx.global Int.sort

  (** The set { a, ..., b } *)
  let range a b =
    let rec loop s i =
      if i > b then
        s
      else
        let i' = Z3.Arithmetic.Integer.mk_numeral_i Ctx.global i in
        let s' = Z3.Set.mk_set_add Ctx.global s i' in
        loop s' (i + 1)
    in
    loop empty a

  (** Set membership *)
  let mem set x = Z3.Set.mk_membership Ctx.global x set
  let subset s t = Z3.Set.mk_subset Ctx.global s t

  let union es = Z3.Set.mk_union Ctx.global es
  let intersection es = Z3.Set.mk_intersection Ctx.global es
  let difference e e' = Z3.Set.mk_difference Ctx.global e e'
end

(* Constructors *)

type expr = Z3.Expr.expr

(** Make an integer variable. *)
let int str = Z3.Arithmetic.Integer.mk_const_s Ctx.global str

(** Create an indexed collection of integer variables. *)
let ints str is =
  let str i = Printf.sprintf "%s%i" str i in
  let syms  = List.map str is in
  List.map int syms


(* Operators *)

let add e e' = Z3.Arithmetic.mk_add Ctx.global [e; e']
let sub e e' = Z3.Arithmetic.mk_sub Ctx.global [e; e']
let mul e e' = Z3.Arithmetic.mk_mul Ctx.global [e; e']
let div e e' = Z3.Arithmetic.mk_div Ctx.global e e' 
let uminus e = Z3.Arithmetic.mk_unary_minus Ctx.global e
let pow e e' = Z3.Arithmetic.mk_power Ctx.global e e'

let equal e e' = Z3.Boolean.mk_eq Ctx.global e e'

let lt e e' = Z3.Arithmetic.mk_lt Ctx.global e e'
let le e e' = Z3.Arithmetic.mk_le Ctx.global e e'
let gt e e' = Z3.Arithmetic.mk_gt Ctx.global e e'
let ge e e' = Z3.Arithmetic.mk_ge Ctx.global e e'

let all es = Z3.Boolean.mk_and Ctx.global es

let distinct es = Z3.Boolean.mk_distinct Ctx.global es

let map f ts = all (List.map f ts)


module DSL = struct
  let ( + )  = add
  let ( - )  = sub
  let ( * )  = mul
  let ( / )  = div
  let ( ~- ) = uminus
  let ( ** ) = pow

  let ( = )  = equal
  let ( < )  = lt
  let ( <= ) = le
  let ( > )  = gt
  let ( >= ) = ge
end


(* Checking *)

type status =
  | Unknown
  | Unsat
  | Sat of { model: (string, int) Hashtbl.t }
[@@deriving show]

module Solver = struct
  let solver ?(ctx = Ctx.global) () = Z3.Solver.mk_solver ctx None

  let add solver constr = Z3.Solver.add solver constr

  let check solver =
    let status = Z3.Solver.check solver [] in

    match status with
    | Z3.Solver.UNKNOWN       -> Unknown
    | Z3.Solver.UNSATISFIABLE -> Unsat
    | Z3.Solver.SATISFIABLE   ->
      let model = Z3.Solver.get_model solver in
      match model with
      | None       -> Unsat
      | Some model ->
        let model = List.map
            (fun x ->
               let sym   = Z3.FuncDecl.get_name x in
               let value = Z3.Model.get_const_interp model x in
               let value = Option.get_exn_or "infallible" value in

               let value =
                 if Z3.Arithmetic.is_int value then
                   let value = Z3.Arithmetic.Integer.get_big_int value in
                   Z.to_int value
                 else
                   failwith "todo: support other datatypes"
               in

               let sym = Z3.Symbol.to_string sym in

               (sym, value))
            (Z3.Model.get_const_decls model)
        in
        Sat { model = Hashtbl.of_list model }
end

