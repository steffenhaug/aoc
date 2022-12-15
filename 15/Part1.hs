import Data.List
import Data.Maybe
import Text.Parsec

import Aoc (int, list2, printall)

input file = fmap (parse sensors file) (readFile file)
  where sensors = sepEndBy sensor newline
        sensor = do
          string "Sensor at "
          sloc <- point <* string ": "
          string "closest beacon is at "
          bloc <- point
          pure $ Sensor sloc bloc
        point = do
          x <- string "x=" *> int <* string ", "
          y <- string "y=" *> int
          pure (x, y)

data Sensor
  = Sensor
      { loc    :: (Int, Int)
      , beacon :: (Int, Int)
      }
  deriving (Show)

data Interval
  = S Int
  | E Int
  deriving (Eq, Show)

instance Ord Interval where
  compare (S a) (S b) = compare a b
  compare (E a) (E b) = compare a b
  compare (S a) (E b) =
    if compare a b == EQ
      then LT
      else compare a b
  compare (E a) (S b) =
    if compare a b == EQ
      then GT
      else compare a b

-- Calculate the area covered by this sensor on the line.
proj y0 sensor =
  if dy > norm || x1 > x2
    then Nothing
    else Just (S x1, E x2)
  where (x, y) = loc sensor
        norm   = manhattan (loc sensor) (beacon sensor)
        dy     = abs (y0 - y)
        dx     = norm - dy

        x1 = if (x - dx, y0) == (beacon sensor)
              then x - dx + 1
              else x - dx

        x2 = if (x + dx, y0) == (beacon sensor)
              then x + dx - 1
              else x + dx

        manhattan v w =
          (abs (fst w - fst v)) + (abs (snd w - snd v))

-- Calculate the area covered by overlapping set by treating it as a parsing problem.
cover intervals = cover' 0 [] intervals
  where
    cover' sofar stack ((S a) : input) =
      cover' sofar ((S a) : stack) input

    cover' sofar ((S a) : []) ((E b) : input) =
      cover' (sofar + m) [] input
      where m =  b - a + 1

    cover' sofar ((S _) : stack) ((E _) : input) =
      cover' sofar stack input

    cover' sofar [] [] = sofar

    cover' sofar s i = undefined


main = do
  Right sensors <- input "input.txt"

  let y         = 2000000
      intervals = catMaybes $ map (proj y) sensors
      flat      = sort $ intervals >>= list2

  print (cover flat)
