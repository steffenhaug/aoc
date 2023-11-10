import           Aoc
import           Data.Char     (ord)
import qualified Data.List     as List
import           Data.Matrix   (Matrix, (!))
import qualified Data.Matrix   as Mat
import           Data.Sequence (Seq, ViewL (EmptyL, (:<)), viewl, (><))
import qualified Data.Sequence as Seq
import           Data.Set      (Set, intersection, union, (\\))
import qualified Data.Set      as Set

input file = do
  txt <- readFile file
  let -- Create heightmap.
      ls = lines txt
      tiles = Mat.fromLists $ map (map elevation) ls
      m = Mat.nrows tiles
      n = Mat.ncols tiles

      -- Figure out where the S and E is based on position in the text.
      Just s = List.elemIndex 'S' txt
      Just e = List.elemIndex 'E' txt
      start = mapt (+ 1) $ s `divMod` (n + 1)
      end = mapt (+ 1) $ e `divMod` (n + 1)

      heightmap =
        Heightmap
          { tiles = tiles,
            m = m,
            n = n
          }

  pure (heightmap, start, end)

data Heightmap = Heightmap
  { tiles :: Matrix Int
  , m     :: Int
  , n     :: Int
  }
  deriving (Show)

inbounds heightmap (k, l) = (1 <= k) && (1 <= l) && (k <= m heightmap) && (l <= n heightmap)

elevation 'S' = elevation 'a'
elevation 'E' = elevation 'z'
elevation chr
  | chr ? ('a', 'z') = ord chr - 97
  | otherwise = undefined
  where
    c ? (a, b) = a <= c && c <= b

neighbors gradient heightmap (i, j) =
  let candidates =
        [ (i + 1, j)
        , (i - 1, j)
        , (i, j + 1)
        , (i, j - 1)
        ]
   in Set.fromList $ (filter accessible) (filter (inbounds heightmap) candidates)
  where
    accessible (k, l) =
      let h0 = tiles heightmap ! (i, j)
          h1 = tiles heightmap ! (k, l)
       in gradient (h1 - h0)

bfs grad target start heightmap =
  let initial_queue = Seq.singleton (start, [])
      initial_visited = Set.singleton (start)
   in bfs' grad target heightmap (viewl initial_queue) initial_visited
  where
    bfs' grad target heightmap EmptyL vis = Nothing
    bfs' grad target heightmap ((v, path) :< queue) vis =
      if target v
        then Just (v : path)
        else
          let adj = neighbors grad heightmap v
              leaves = [(x, v : path) | x <- Set.toList (adj \\ vis)]
              queue' = queue >< Seq.fromList leaves
              vis' = vis `union` adj
           in bfs' grad target heightmap (viewl queue') vis'

main = do
  (heightmap, start, end) <- input "input.txt"

  -- Part One.
  let Just path = bfs (<= 1) (== end) start heightmap
  putStrLn $ "Part one: " ++ (show $ length path - 1)

  -- Part Two.
  let Just path = bfs (>= -1) (\p -> (tiles heightmap ! p) == 0) end heightmap
  putStrLn $ "Part two: " ++ (show $ length path - 1)
