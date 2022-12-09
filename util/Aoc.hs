module Aoc where

printall :: [a] -> IO ()
printall = mapM_ print
