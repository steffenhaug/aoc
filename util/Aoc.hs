module Aoc where

import Text.ParserCombinators.Parsec

intp :: Parser Int
intp = do
  sign <- option ' ' (char '-')
  digs <- many1 digit
  return $ read (sign:digs)

yo = "Hello, Nix!"

printall :: Show a => [a] -> IO ()
printall = mapM_ print
