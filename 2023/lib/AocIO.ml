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
  let start ?(m=0) g = Group.start g m
  let stop ?(m=0) g = Group.stop g m
  let str ?(m=0) g = Group.get g m
      
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
  open Effect
         
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

  (* Parser Combinators *)
  
  type input = token Seq.t
  type _ Effect.t += Consume : token option Effect.t
                  |  Peek    : token option Effect.t
  exception ConsumeEmpty
  exception UnexpectedToken of token
  exception UnexpectedEof
    
  let run p (input: input) = 
    let input = ref input in
    Effect.Deep.match_with p ()
      { effc = (fun (type a) (eff: a Effect.t) ->
            match eff with
            | Consume -> Some (fun (k: (a, _) Effect.Deep.continuation) ->
                let next = Seq.head !input in
                input := Seq.tail_exn !input;
                Effect.Deep.continue k next)
            | Peek -> Some (fun (k: (a, _) Effect.Deep.continuation) ->
                let next = Seq.head !input in
                Effect.Deep.continue k next)
            | _ -> None);
        exnc =
          (function
            | ConsumeEmpty -> None
            | e -> raise e);
        retc = fun v -> Some v;
      }
      
  let run_exn p (input: input) = 
    let input = ref input in
    Effect.Deep.match_with p ()
      { effc = (fun (type a) (eff: a Effect.t) ->
            match eff with
            | Consume -> Some (fun (k: (a, _) Effect.Deep.continuation) ->
                let next = Seq.head !input in
                input := Seq.tail_exn !input;
                Effect.Deep.continue k next)
            | Peek -> Some (fun (k: (a, _) Effect.Deep.continuation) ->
                let next = Seq.head !input in
                Effect.Deep.continue k next)
            | _ -> None);
        exnc = raise;
        retc = fun v -> v;
      }
      
  let peek () = perform Peek
      
  let next () =
    match perform Consume with
    | Some tok -> tok
    | None     -> raise ConsumeEmpty

  let expect tag =
    match perform Consume with
    | Some tok ->
      if (is tag) tok then
        ()
      else raise (UnexpectedToken tok)
    | None -> raise UnexpectedEof

  let consume tag =
    match perform Consume with
    | Some tok ->
      if (is tag) tok then
        tok.str
      else raise (UnexpectedToken tok)
    | None -> raise UnexpectedEof

  let maybe tag =
    match perform Consume with
    | Some tok ->
      if (is tag) tok then
        expect tag
      else ()
    | None -> ()

  let lookat tag =
    match perform Peek with
    | Some tok ->
      if (is tag) tok then true
      else false
    | None -> false

  (* repeat , p
     works like
     l ::= p
         | p, l
  *)
  let sepby sep p =
    let rec loop els =
      let els' = Queue.snoc els (p ()) in
      if lookat sep then
        begin
          expect sep;
          loop els'
        end
      else
        els'
    in 
    
    Queue.to_list (loop Queue.empty)

end
