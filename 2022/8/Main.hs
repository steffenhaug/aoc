import Data.Char (digitToInt)
import Data.List (findIndex)
import Data.Matrix (Matrix, mapPos, submatrix, (!))
import qualified Data.Matrix as Mat
import Data.Maybe (fromMaybe)

input f = do
  txt <- readFile f
  let ls = lines txt
  let trees = map (map digitToInt) ls
  return (Mat.fromLists trees)

-- Safe max function.
max' [] = minBound
max' xs = maximum xs

-- Easy way to add alternative to a Maybe.
(?) = flip fromMaybe

-- Get the line of sight in every direction from a given tree.
los mat (i, j) =
  let u = [mat ! (k, j) | k <- i ↘ 1]
      d = [mat ! (k, j) | k <- i ↗ m]
      l = [mat ! (i, k) | k <- j ↘ 1]
      r = [mat ! (i, k) | k <- j ↗ n]
   in [u, d, l, r]
  where
    a ↘ b = [a - 1, a - 2 .. b]
    a ↗ b = [a + 1 .. b]
    m = Mat.ncols mat
    n = Mat.nrows mat

-- Is the tree at position ij with height aij visible?
visibility forest ij tree =
  let surrounding = map max' (los forest ij)
   in any (< tree) surrounding

-- The scenic score of the tree at position ij.
scenic_score forest ij tree =
  let score ts =
        let pts = fmap (+ 1) $ findIndex (>= tree) ts
         in pts ? (length ts)
      surrounding = map score (los forest ij)
   in product surrounding

main = do
  forest <- input "input.txt"

  -- Part one.
  let visible = mapPos (visibility forest) forest
  print $ (sum . map fromEnum . Mat.toList) visible

  -- Part two.
  let scores = mapPos (scenic_score forest) forest
  print (maximum scores)
