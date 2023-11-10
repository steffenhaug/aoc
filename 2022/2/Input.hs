module Input
  ( input,
    Move (Rock, Paper, Scissors),
    Result (Loss, Draw, Win),
    Plan,
  )
where

import Text.ParserCombinators.Parsec

input :: String -> IO (Either ParseError Plan)
input file = do
  txt <- readFile file
  let parsed = parse plan "Plan Parser" txt
  return parsed

data Move = Paper | Scissors | Rock deriving (Show)

data Result = Loss | Draw | Win deriving (Show)

type Plan = [(Move, Result)]

move :: Parser Move
move =
  (char 'A' >> return Rock)
    <|> (char 'B' >> return Paper)
    <|> (char 'C' >> return Scissors)

result :: Parser Result
result =
  (char 'X' >> return Loss)
    <|> (char 'Y' >> return Draw)
    <|> (char 'Z' >> return Win)

turn :: Parser (Move, Result)
turn = do
  op_move <- move
  space
  res <- result
  return (op_move, res)

plan :: Parser [(Move, Result)]
plan = do
  turns <- sepEndBy turn newline
  eof
  return turns
