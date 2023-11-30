module Alco = Alcotest
open Containers
open Aoc

module Parser = struct
  module Tag = struct
    type t =
      | L
      | R
      | S
    [@@deriving show, eq]

    let table =
      [ "\\(",    L;
        "\\)",    R;
        "[a-z]+", S;
      ]
  end

  module Tok = Parse.Tokenizer(Tag)
end

let token = Parser.Tok.(Alcotest.testable pp_token equal_token)

let tokenize () =
  let src = "(ex am ple)" in
  let input = Parser.Tok.tokenize src in

  Alco.(check token) "tokens match"
    (Result.get_exn (Parser.Tok.next input))
    { tag = Parser.Tag.L; pos =  0; str = "(" };

  List.iter
    (fun tok ->
       Alco.(check token) "tokens match" tok (Result.get_exn (Parser.Tok.consume input)))
    [ { tag = Parser.Tag.L; pos =  0; str = "(" };
      { tag = Parser.Tag.S; pos =  1; str = "ex" };
      { tag = Parser.Tag.S; pos =  4; str = "am" };
      { tag = Parser.Tag.S; pos =  7; str = "ple" };
      { tag = Parser.Tag.R; pos = 10; str = ")" };
    ];

  Alco.(check (option token)) "stream is now empty" None (Parser.Tok.peek input)

let () = 
  let quick name fn = Alco.test_case name `Quick fn in
  Alco.run
    "Parser"
    [ "Tokenizer",
      [ quick "tokenize simple strings" tokenize;
      ]
    ]
