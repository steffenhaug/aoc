module Aoc where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Graphics.Image (Image, Pixel (PixelRGB), RGB, VU, makeImage)
import qualified Graphics.Image as Im
import Text.ParserCombinators.Parsec

-- Parse an integer with an optional minus sign.
int :: Parser Int
int = do
  sign <- option ' ' (char '-')
  digs <- many1 digit
  return $ read (sign : digs)

-- Parse a natural number.
nat :: Parser Int
nat = read <$> many1 digit

parselines :: Parser a -> String -> Either ParseError [a]
parselines p = parse (sepEndBy1 p newline) "Parse Lines"

-- Print every element of a list.
printall :: (Traversable t, Show a) => t a -> IO ()
printall = mapM_ print

-- Get every N elements of a list.
-- This includes the first element, i. e. it gets element 0, N, 2N, and so on.
every n [] = []
every n (x : xs) = x : (every n (drop (n - 1) xs))

-- Push elements of xs on to ys in a stack fashion.
revcat xs ys = foldl (flip (:)) ys xs

-- Rasterize a row-major IntMap to an image.
rasterize :: Double -> (Int, Int) -> IntMap (Double, Double, Double) -> Image VU RGB Double
rasterize scale (m, n) pixels =
  Im.scale Im.Nearest Im.Edge (scale, scale) $
    Im.makeImage (m, n) getpixel
  where
    getpixel (i, j) =
      let (r, g, b) = IM.findWithDefault (0, 0, 0) (n * i + j) pixels
       in (PixelRGB r g b)

plot :: String -> Double -> (Int, Int) -> IntMap (Double, Double, Double) -> IO ()
plot name scale (m, n) pixels = do
  let im = rasterize scale (m, n) pixels
  Im.writeImage name im

-- Invariant: x < y < z.
max3 :: (Bounded a, Ord a) => [a] -> (a, a, a)
max3 = max3' (minBound, minBound, minBound)
  where
    max3' (x, y, z) [] = (x, y, z)
    max3' (x, y, z) (t : ts)
      | z < t = max3' (y, z, t) ts
      | y < t = max3' (y, t, z) ts
      | x < t = max3' (t, y, z) ts
      | otherwise = max3' (x, y, z) ts

max2 :: (Bounded a, Ord a) => [a] -> (a, a)
max2 = max2' (minBound, minBound)
  where
    max2' (x, y) [] = (x, y)
    max2' (x, y) (t : ts)
      | y < t = max2' (y, t) ts
      | x < t = max2' (t, y) ts
      | otherwise = max2' (x, y) ts

-- For use with IntMap.alter to maintain a map of counters.
bump x Nothing = Just x
bump x (Just n) = Just (n + x)

-- Tuple maps
mapt f (a, b) = (f a, f b)

mapt3 f (a, b, c) = (f a, f b, f c)

list2 (a, b) = [a, b]

list3 (a, b, c) = [a, b, c]

-- Returns tuples of consecutive pairs in a list.
pairs xs = zip xs (drop 1 xs)
