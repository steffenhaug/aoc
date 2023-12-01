open Containers
include CCIO
module Queue = CCFQueue

(* Helper functions. *)

let read ch = CCIO.read_all ch
let readlines ch = CCIO.read_lines_l ch

(* Simple parsing. *)

let rx str = Re.compile (Re.Pcre.re str)

let ints input =
  let ints = Re.matches (rx "\\d+") input in
  List.map Int.of_string_exn ints

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



(** Utilities for printing formatted strings. *)
module Fmt = struct
    include CCFormat
    let _ = set_color_default true

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

module Tokenizer(T: Tag) = struct
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

  (** Check if a token option has a given tag. *)
  let is' tag tok = 
    match tag, tok with
    | None, None         -> true
    | Some tag, Some tok -> is tag tok
    | _                  -> false

  let tokenize input =
    let input  = Queue.of_list (Re.all lexer_rx input) in

    (* Extract token information from the lexed input. *)
    let tokens = Queue.map
        (fun group ->
           let str = Re.Group.get group 0 in
           let pos = Re.Group.start group 0 in
           let tag = snd (List.find (fun (rx, _) -> Re.execp rx str) rx_table) in
           { tag; pos; str; })
        input
    in

    ref tokens

  let peek input = Queue.first !input

  let next input =
    match peek input with
    | None   -> Error "unexpected end of input"
    | Some t -> Ok t

  let consume input =
    let+ next = next input in
    input := Queue.tail !input;
    next

  let expect tag input =
    let* tok = consume input in
    if is tag tok then Ok ()
    else Error "unexpected token"

  let repeat ?(until = None) parse input =
    let rec loop els =
      if is' until (peek input) then
        Ok els
      else
        let* a = parse input in
        loop (Queue.snoc els a)
    in
    let+ elements = (loop Queue.empty) in
    Queue.to_list elements
end
