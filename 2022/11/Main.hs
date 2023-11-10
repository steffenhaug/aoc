import Aoc
import Control.Monad.State
import Data.IntMap (IntMap)
import Data.IntMap as IM
import Debug.Trace
import Input

type Monkeys = IntMap Monkey
type Inspections = IntMap Int

throw lcm (t@Monkey {m_items = []}) f1 f2 =
  (t, f1, f2)
throw lcm (t@Monkey {m_items = i : is}) f1 f2 =
  let worry = (m_op t) i `mod` lcm
      f1' = f1 {m_items = worry : m_items f1}
      f2' = f2 {m_items = worry : m_items f2}
      divisor = m_div t
   in if worry `mod` divisor == 0
        then throw' f1' f2
        else throw' f1 f2'
  where
    throw' = throw lcm t {m_items = is}

inspect_items :: Int -> Int -> State (Monkeys, Inspections) ()
inspect_items lcm monkey = do
  (monkeys, inspections) <- get

  let t = monkeys ! monkey

      -- Find the friends of the thrower, and go through the items.
      friends = m_friends t
      f1 = monkeys ! fst friends
      f2 = monkeys ! snd friends
      (t', f1', f2') = throw lcm t f1 f2

      -- Insert the changes into the monkey state.
      monkeys' =
        (IM.insert (m_id t') t')
          . (IM.insert (m_id f1') f1')
          . (IM.insert (m_id f2') f2')
          $ monkeys

      -- Bump the throwers # of inspected items.
      n = length (m_items t)
      inspections' = alter (bump n) monkey inspections

  put (monkeys', inspections')
  pure ()

do_round :: Int -> State (Monkeys, Inspections) ()
do_round lcm = do
  (monkeys, inspections) <- get

  -- Perform monkey inspections for all the monkeys.
  let positions = IM.keys monkeys
  mapM_ (inspect_items lcm) positions

  pure ()

main = do
  monkeys <- input "test.txt"

  let lcm = product (IM.map m_div monkeys)
      res = execState (mapM (const $ do_round lcm) [1 .. 10000]) (monkeys, empty)
      most_active = max2 (elems (snd res))
      monkey_business = fst most_active * snd most_active

  putStrLn $ "Part Two: " ++ (show monkey_business)
