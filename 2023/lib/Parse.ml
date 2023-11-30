open Containers
module Queue = CCFQueue

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