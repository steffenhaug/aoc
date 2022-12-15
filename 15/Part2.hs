import Data.SBV
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

manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

find_beacon sensors = sat $ do
  [x, y] <- sIntegers ["x", "y"]

  let addsensor (Sensor (sx, sy) (bx, by)) = constrain $ dx + dy .> r
        where dx = abs (fromIntegral sx - x)
              dy = abs (fromIntegral sy - y)
              r  = fromIntegral (manhattan (sx, sy) (bx, by))

  traverse addsensor sensors

  constrain $ x .>= 0
  constrain $ y .>= 0
  constrain $ x .<= 4000000
  constrain $ y .<= 4000000

main = do
  Right sensors <- input "input.txt"

  sol' <- find_beacon sensors

  let Just x = getModelValue "x" sol' :: Maybe Integer
      Just y = getModelValue "y" sol' :: Maybe Integer
      tuning_frequency = 4000000*x + y

  print tuning_frequency
