import Data.List
import Text.ParserCombinators.Parsec

import Aoc (list2, nat)

input file = do
  txt <- readFile file
  pure $ parse (pairs <* eof) "AoC Input Parser" txt
  where
    pairs = pair `sepBy` newline
    pair = do
      p1 <- packet <* newline
      p2 <- packet <* newline
      pure (p1, p2)
    packet =
      choice
        [ I <$> nat
        , L <$> list
        ]
    list = (char '[') *> (packet `sepBy` (char ',')) <* (char ']')

data Packet
  = I Int
  | L [Packet]
  deriving (Eq, Show)

instance Ord Packet where
  -- Rule #1.
  compare (I l) (I r) = compare l r
  -- Rule #2.
  compare (L (l : ls)) (L (r : rs)) =
    case compare l r of
      EQ   -> compare ls rs
      ineq -> ineq
  compare (L [])      (L (_ : _)) = LT -- Left runs out first.
  compare (L (_ : _)) (L [])      = GT -- Right runs out first.
  compare (L [])      (L [])      = EQ -- They run out at the same time.
  -- Rule #3.
  compare (I l)  (L rs) = compare (L [(I l)]) (L rs)
  compare (L ls) (I r)  = compare (L ls) (L [I r])

main = do
  Right packets <- input "input.txt"

  -- Part one.
  let indices = map (+ 1) (findIndices (uncurry (<)) packets)
  print $ sum indices

  -- Part two.
  let d1 = (L [L [I 2]])
      d2 = (L [L [I 6]])
      message = sort $ d1 : d2 : (packets >>= list2)
      Just p1 = fmap (+ 1) (elemIndex d1 message)
      Just p2 = fmap (+ 1) (elemIndex d2 message)
      decoder_key = p1 * p2

  print decoder_key
