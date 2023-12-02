open Containers
include CCIO
module Queue = CCFQueue

(* Helper functions. *)

let read ch = CCIO.read_all ch
let readlines ch = CCIO.read_lines_l ch

(* Simple parsing. *)



module Re = struct
  include Re
  let compiled str = Re.compile (Re.Pcre.re str)

  let ints_rx = compiled "\\d+"
  let ints input =
    let ints = Re.matches ints_rx input in
    List.map Int.of_string_exn ints

  let int input = 
    let i = Re.Group.get (Re.exec ints_rx input) 0 in
    Int.of_string_exn i

  (** Dirty O(n^2) overlapping regex search *)
  let match_overlapping rx input =
    let vec = Vector.create () in
    let rec scan pos =
      match Re.exec_opt ~pos:pos rx input with
      | None   -> ()
      | Some g ->
        let resume = 1 + Re.Group.start g 0 in
        let m      =     Re.Group.get   g 0 in
        Vector.push vec m;
        scan resume
    in scan 0;
    vec
end

module Str = struct
  include CCString

  let split ?(at = " ") str = CCString.split ~by:at str
  let split2 ?(at = " ") str =
    let before    = CCString.find ~sub:at str in
    let after     = before + CCString.length at in
    let remaining = CCString.length str - after in

    let before = CCString.sub str 0 before in
    let after  = CCString.sub str after remaining in

    before, after
end


(** Utilities for printing formatted strings. *)
module Fmt = struct
    include CCFormat

    let list
        ?(sep = return "; ")
        ?(a = "[")
        ?(b = "]")
        p
      = CCFormat.within a b (CCFormat.list ~sep:sep p)
end


(* Quick and dirty Lexer generator. *)

module type Tag = sig
  type t
  val table : (string * t) list
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Yak(T: Tag) = struct
  open Re
  open Containers
  open Result.Infix

  type token =
    { tag: T.t;
      pos: int;
      str: string;
    }
  [@@deriving show, eq]

  (* Compile the regexes we need. *)
  let rx_table, lexer_rx =
    let tab = List.map (fun (rx, tag) -> Pcre.re rx, tag) T.table in
    let rx  = Re.compile (Re.alt (List.Assoc.keys tab)) in
    let tab = List.map (fun (pcre, tag) -> Re.compile pcre, tag) tab in
    tab, rx

  (** Check if a token has a given tag. *)
  let is tag tok = T.equal tok.tag tag

  (** Turn a string into a token stream. *)
  let tokenize input =
    let input  = Re.all lexer_rx input in

    (* Extract token information from the lexed input. *)
    let tokens = List.map
        (fun group ->
           let str = Re.Group.get group 0 in
           let pos = Re.Group.start group 0 in
           let tag = snd (List.find (fun (rx, _) -> Re.execp rx str) rx_table) in
           { tag; pos; str; })
        input
    in

    Seq.of_list tokens

  type input = token Seq.t
  type 'a parser = input -> ('a * input) list






end
