{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Int (Int64)
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

-- we'll need: oscillator, register, memory (loaded from a file), wires/busses, basic logic gates, metastable
-- gluing these components together are wires and 
-- the components above have inputs and outputs, which connect to/from wires; when their inputs change, their outputs may change after a gate delay
-- when inputs to a wire change, its new state is calculated (or blue smoke escapes), and it outputs are signalled to update after a time delay

data Signal
  = DH
  | DL
  | PU
  | PD
  | HZ
  deriving (Eq, Show)


type Picoseconds = Int64
nano, micro, milli :: Int64
nano = 1000
micro = 1000000
milli = 1000000000





-- hmmmm, so what I really want is just a simple array of signals
-- the signal array will represent input/output pins
-- wires will be represented by two sets of indexes
-- I'll also have a map of scheduled changes to signals
-- to implement oscillators, I'll also need to have something extra that might add extra changes into the schedule before we take the next slice of time



type Comp = ([Int], [Signal]->[(Picoseconds, Signal)] ,[Int]) -- inputs, update logic, outputs
type Comps = [Comp]
type Schedule = Map Picoseconds [(Int, Signal)]
data M = M
  { components :: [Comp]
  , pins :: [Signal]
  , schedule :: Schedule
  , time :: Picoseconds
  }

main :: IO ()
main = do
  let m0 = M
        { components = [([0], clkLogic, [0])]
        , pins = [HZ]
        , schedule = Map.fromList [(0, [(0, DL)])]
        , time = 0
        }
  putStrLn "hello"
  putStrLn $ concat [show m0.time, " (", show $ Map.size m0.schedule, " queued): ", show m0.pins]
  let m1 = step m0
  putStrLn $ concat [show m1.time, " (", show $ Map.size m1.schedule, " queued): ", show m1.pins]
  let m2 = step m1
  putStrLn $ concat [show m2.time, " (", show $ Map.size m2.schedule, " queued): ", show m2.pins]
  let m3 = step m2
  putStrLn $ concat [show m3.time, " (", show $ Map.size m3.schedule, " queued): ", show m3.pins]
  where
  clkLogic [DL] = [(1000*milli, DH)]
  clkLogic [DH] = [(1000*milli, DL)]

step :: M -> M
step m =
      -- get all the next signal change events
  let (schedule', t, evs) = nextEvents m.schedule
      -- write those events to the pins
      pins' = foldl writeSignal m.pins evs
      -- run logic where written pins are inputs, generating new signal change events
      triggeredComps = triggerComps m.components evs
      evs' = Map.unionsWith (<>) $ updateComp t pins' <$> triggeredComps
      -- add the logic's new events into the schedule
      schedule'' = Map.unionWith (<>) schedule' evs'
   in m
      { pins = pins'
      , schedule = schedule''
      , time = t
      }

nextEvents :: Schedule -> (Schedule, Picoseconds, [(Int, Signal)])
nextEvents sched = case Map.assocs sched of
  [] -> (sched, maxBound, [])
  ((t, evs):_) -> (Map.delete t sched, t, evs)

writeSignal :: [Signal] -> (Int, Signal) -> [Signal]
writeSignal st (i,s') = take i st <> [s'] <> drop (i + 1) st

triggerComps :: [Comp] -> [(Int, Signal)] -> [Comp]
triggerComps comps evs =
  let updatedPins = Set.fromList $ fst <$> evs
   in filter (isUpdated updatedPins) comps
  where
  isUpdated :: Set Int -> ([Int], a, b) -> Bool
  isUpdated pins (iPins, _, _) = any (`Set.member`pins) iPins

updateComp :: Picoseconds -> [Signal] -> Comp -> Schedule
updateComp t allPins (iPins, logic, oPins) =
  let inps = map (allPins !!) iPins
      outs = logic inps
   in Map.unionsWith (<>) $ zipWith mkEvent outs oPins
  where
  mkEvent :: (Picoseconds, Signal) -> Int -> Schedule
  mkEvent (dt, s) oPin = Map.singleton (t+dt) [(oPin,s)]


-- Now that I have this stupid thing, there are some obvious improvements:
-- first, use arrays; also store an enum of component logic instead of a function; but all that's internal
--
-- also, stop conditions (time too large, schedule complete, whatever)
-- the real things are:
--   * pins should be equipped with an initialization function, which might draw on random numbers
--   * right now, you just have to be correct with global pin numberings
--     * make a monad that gives names to pins of/within a component
--     * the monad should also allow wiring between names
--     * when the monad runs, it should unify pins, aggregating their names, and assign indices to each (unified) pin
-- when I output, it should be in a binary format so it doesn't take too much time