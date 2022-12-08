import Data.Char (digitToInt)
import qualified Data.Matrix as Mat
import Data.Vector (Vector, slice, (!))
import qualified Data.Vector as Vec

input f = do
  txt <- readFile f
  let ls = lines txt
  let trees = map (map digitToInt) ls
  return (Mat.fromLists trees)

-- Safe maximum of a vector.
max' vec
  | Vec.null vec = minBound
  | otherwise = Vec.maximum vec

visibility mat =
  let m = Mat.nrows mat
      n = Mat.ncols mat
      vis' (i, j) tree =
        let r = Mat.getRow i mat
            c = Mat.getCol j mat
            -- Find the biggest tree in each cardinal direction.
            north = max' $ (slice 0 (i - 1)) c
            south = max' $ (slice i (m - i)) c
            west = max' $ (slice 0 (j - 1)) r
            east = max' $ (slice j (n - j)) r
         in or [tree > north, tree > south, tree > west, tree > east]
   in Mat.mapPos vis' mat

one = do
  trees <- input "input.txt"
  let vis = visibility trees
  let count = (sum . map fromEnum) (Mat.toList vis)
  print count

scenic_score mat =
  let m = Mat.nrows mat
      n = Mat.ncols mat
      vis' (i, j) tree =
        let r = Mat.getRow i mat
            c = Mat.getCol j mat
            score ts =
              maybe (Vec.length ts) id $
                fmap (+ 1) (Vec.findIndex (>= tree) ts)
            -- Find the biggest tree in each cardinal direction.
            north = score $ Vec.reverse $ (slice 0 (i - 1)) c
            south = score $ (slice i (m - i)) c
            west = score $ Vec.reverse $ (slice 0 (j - 1)) r
            east = score $ (slice j (n - j)) r
         in north * south * west * east
   in Mat.mapPos vis' mat

two = do
  trees <- input "input.txt"
  let score = scenic_score trees
  let best = maximum (Mat.toList score)
  print best
