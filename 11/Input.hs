module Input where

import Aoc
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Text.ParserCombinators.Parsec

input file = do
  txt <- readFile file
  let Right parsed = parse (sepBy1 monkeyp $ string "\n") "AoC Input Parser" txt
  let monkeys = IM.fromList $ map (\m -> (m_id m, m)) parsed
  pure monkeys

monkeyp = do
  n <- do string "Monkey "; n <- natp; string ":\n"; pure n
  items <- itemsp
  op <- operationp
  Test divisor friends <- testp
  pure
    Monkey
      { m_id = n,
        m_items = items,
        m_op = op,
        m_div = divisor,
        m_friends = friends
      }

itemsp = do
  string "  Starting items: "
  items <- sepBy1 natp (string ", ")
  newline
  pure items

operationp = do
  string "  Operation: new = old "
  operator <-
    choice
      [ do string "* "; pure (*),
        do string "+ "; pure (+)
      ]
  op <-
    choice
      [ do i <- natp; pure $ (operator i),
        do string "old"; pure $ (\x -> operator x x)
      ]
  newline
  pure op

testp = do
  divisor <- do string "  Test: divisible by "; i <- natp; newline; pure i
  iftrue <- do string "    If true: throw to monkey "; m <- natp; newline; pure m
  iffalse <- do string "    If false: throw to monkey "; m <- natp; newline; pure m
  pure $ Test divisor (iftrue, iffalse)

data Monkey = Monkey
  { m_id :: Int,
    m_items :: [Int],
    m_op :: Int -> Int,
    m_div :: Int,
    m_friends :: (Int, Int)
  }

data Test = Test Int (Int, Int) deriving (Show)
