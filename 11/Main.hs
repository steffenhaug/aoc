import Aoc
import Control.Monad.State
import Data.IntMap (IntMap)
import Data.IntMap as IM
import Debug.Trace
import Input

type Monkeys = IntMap Monkey

type Inspections = IntMap Int

bump Nothing = Just 1
bump (Just n) = Just (n + 1)

inspect_items :: Int -> Int -> State (Monkeys, Inspections) ()
inspect_items mod' position = do
  (monkeys, inspections) <- get

  let thrower = monkeys ! position
      items = m_items thrower

  case items of
    [] -> pure ()
    (i : is) ->
      let op = m_op thrower
          i' = case op of
            Unary f -> f i
            Binary f -> f i i
          worry = i' `mod` mod'
          Test divisor if_t if_f = m_test thrower
          throw_to =
            if worry `mod` divisor == 0
              then if_t
              else if_f

          receiver = monkeys ! throw_to
          receiver' = receiver {m_items = worry : (m_items receiver)}
          thrower' = thrower {m_items = is}

          monkeys' =
            (IM.insert (m_id thrower) thrower')
              . (IM.insert (m_id receiver) receiver')
              $ monkeys

          inspections' = IM.alter bump (m_id thrower) inspections
       in do
            put (monkeys', inspections')
            inspect_items mod' position
            pure ()

do_round :: Int -> State (Monkeys, Inspections) ()
do_round mod' = do
  (monkeys, inspections) <- get

  -- Perform monkey inspections.
  let positions = IM.keys monkeys
  mapM_ (inspect_items mod') positions

  pure ()

main = do
  monkeys <- input "input.txt"
  let mod' = product (IM.map (\Monkey {m_test = Test div _ _} -> div) monkeys)
      (monkeys', inspections) = execState (mapM (const $ do_round mod') [1 .. 10000]) (monkeys, IM.empty)
      most_active = (max2 $ elems inspections)
      monkey_business = fst most_active * snd most_active

  print most_active
  print monkey_business
