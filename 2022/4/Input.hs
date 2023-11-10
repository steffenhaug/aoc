module Input
  ( input,
  Pair (Assignments)
  )
where

import Text.ParserCombinators.Parsec

data Pair = Assignments (Int, Int) (Int, Int)
  deriving (Show)

input :: String -> IO (Either ParseError [Pair])
input file = do
  txt <- readFile file
  let parsed = parse pairs "AoC Input" txt
  return parsed

pairs :: Parser [Pair]
pairs = do
  p <- sepEndBy pair newline
  eof
  return p

range :: Parser (Int, Int)
range = do
  beg <- read <$> (many1 digit)
  char '-'
  end <- read <$> (many1 digit)
  return (beg, end)

pair :: Parser Pair
pair = do
  fst <- range
  char ','
  snd <- range
  return (Assignments fst snd)
