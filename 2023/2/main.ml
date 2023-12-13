open Aoc
open Aoc.IO

type round = (string, int) Hashtbl.t
[@@deriving show]

type game =
  { id:     int;
    rounds: round list;
  }
[@@deriving show]

module Parser = struct
  module Tok = struct
    type t = Game | Int | Color | Colon | Semi | Comma
    [@@deriving show, eq]
             
    let table =
      [ "Game",           Game;
        "red|green|blue", Color;
        ":",              Colon;
        ",",              Comma;
        ";",              Semi;
        "\\d+",           Int;
      ]
  end

  module Yak = Yak(Tok)
  open Yak
  open Tok
      
  let rec parse line =
    let tok = Yak.tokenize(line) in
    Yak.run_exn game tok
    
  and game() =
    expect Game;
    let id = consume Int in
    expect Colon;
    { id     = Re.int id;
      rounds = rounds();
    }

  and rounds() = sepby Semi draws
    
  and draws() =
    let draws =
      sepby Comma
        (fun () ->
           let n     = consume Int in
           let color = consume Color in
           (color, Re.int n))
    in Hashtbl.of_list draws
end


let pow { id; rounds } =
  let rec loop rounds r' g' b' =
    match rounds with
    | []         -> r' * g' * b'
    | tab :: rem ->
      let r = Hashtbl.get_or ~default:1 tab "red" in
      let g = Hashtbl.get_or ~default:1 tab "green" in
      let b = Hashtbl.get_or ~default:1 tab "blue" in
      loop rem (max r r') (max g g') (max b b')
  in
  loop rounds 1 1 1

  
let () = 
  let input = readlines stdin in
  try
    let games = List.map Parser.parse input in
    let pows = List.map pow games in
    let sum xs = List.fold_right ( + ) xs 0 in
    Fmt.(pr "%i\n") (sum pows);     
  with
  | Parser.Yak.UnexpectedToken tok ->
    Fmt.(pr "Unexpected Token %a\n" Parser.Yak.pp_token) tok;
