import Aoc (nat, printall)
import Control.Monad.State
import Data.Bits
import qualified Data.List as List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Text.Parsec hiding (State)

{- Shotout to this guy:
 -  https://github.com/juanplopes/advent-of-code-2022/blob/main/day16.py
 - For making me realize the critical information:
 -  After Floyd-Warshalling the graph, you ONLY ever visit a node once
 -  in a correct solution, which makes drastically reduces the search space.
 -}

input file = do
  Right parsed <- fmap (parse valves file) (readFile file)
  let (vl, el) = unzip parsed
      -- Create fast lookup table of valve data.
      v = Map.fromList $ zip [1..] vl
      n = length v
      -- Create edge set since its easier to simplify.
      connected i j =
        let out = el !! (i - 1)
            w   = name (v ! j)
         in w `elem` out
      adj = Map.fromList
              [((i, j), 1) | i <- Map.keys v
                           , j <- Map.keys v
                           , connected i j ]

      -- Find the start node.
      Just s = List.findIndex ((=="AA") . name) vl
      start = 1 + s
  -- Return the parsed graph.
  pure $ Graph adj v n start
  where valves = sepEndBy valve newline
        valve = do
          name <- string "Valve " *> many upper
          flow <- string " has flow rate=" *> nat <* string "; "
          choice [ try $ string "tunnels lead to valves "
                 , try $ string "tunnel leads to valve "
                 ]
          edges <-  (sepBy (many upper) (string ", "))
          pure (Valve name flow, edges)


data Valve
  = Valve
      { name :: String
      , flow :: Int
      }
  deriving (Show)

type Adj = Map (Int, Int) Int

data Graph
  = Graph
      { adj    :: Adj
      , valves :: Map Int Valve
      , nvs    :: Int
      , start  :: Int
      }
  deriving (Show)


floyd_warshall g =
  let initial = Map.fromList
        [((i, j), if (i, j) `Map.member` adj g then 1
                  else if i == j then 0
                  else 666)
          | i <- [1..nvs g]
          , j <- [1..nvs g]]
   in g { adj=execState fw initial }
  where fw = do
          let update :: (Int, Int, Int) -> State Adj ()
              update (i, j, k) = do
                e_ij <- gets (! (i, j))
                e_ik <- gets (! (i, k))
                e_kj <- gets (! (k, j))
                modify $ Map.insert (i, j) $ min e_ij (e_ik+e_kj)
          traverse update [(i, j, k) | k <- [1..nvs g]
                                     , j <- [1..nvs g]
                                     , i <- [1..nvs g]]


search :: Graph -> Int -> Map (Set Int) Int
search graph t = execState (search' graph t Set.empty 0 (start graph)) Map.empty
  where
    search' g t open f v =
      -- If we are out of time, don't do anything more.
      if t <= 0 then pure ()
      else do
        -- Keep track of the best possible flow for a given set of open vents.
        max_f <- gets $ (Map.findWithDefault 0) open
        modify $ Map.insert open (max max_f f)

        -- Visit remaining candidates.
        let candidates = Map.filter ((/= 0) . flow) (valves g)
            bithacking w _ = w `Set.notMember` open
            unvisited = Map.filterWithKey bithacking candidates

            visit :: Int -> State (Map (Set Int) Int) ()
            visit u =
              let t' = t - (adj g ! (v, u)) - 1
                  f' = f + t' * (flow (valves g ! u))
               in search' g t' (Set.insert u open) f' u

        mapM_ visit (Map.keys unvisited)

main = do
  graph <- input "input.txt"

  let graph'  = floyd_warshall graph
      visited = search graph' 30

  print $ maximum (Map.elems visited)
