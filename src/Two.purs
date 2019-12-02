module Two where

import Data.Array
import Data.Maybe
import Data.Show
import Debug.Trace
import Prelude

import Partial.Unsafe (unsafePartial)

type MemState = Array Int

type IP = Int

data Status = Running | Halted

instance statusShow :: Show Status where
  show Running = "Running"
  show Halted = "Halted"

data ProgState = ProgState MemState IP Status

instance progstateshow :: Show ProgState where
  show (ProgState prog pos status) = "ProgState " <>
                                     show prog <> " "
                                     <> show pos <> " "
                                     <> show status

step :: Partial => ProgState -> ProgState
step (ProgState prog pos Halted) = (ProgState prog pos Halted)
step (ProgState prog pos Running) =
  if index prog pos == Just 99
  then (ProgState prog pos Halted)
  else let instruction = slice pos (pos + 4) prog
           [op, a, b, dst] = instruction
           Just aval = index prog a
           Just bval = index prog b
           val = case op of
             1 -> aval + bval
             2 -> aval * bval
           Just newstate = modifyAt dst (const val) prog
       in ProgState newstate (pos + 4) Running

runProg :: MemState -> MemState
runProg p = runProg' (ProgState p 0 Running)
  where runProg' ps@(ProgState m _ Halted) = m
        runProg' ps@(ProgState _ _ Running) = runProg' $ unsafePartial step ps

program :: MemState
program = [3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,2,19,6,23,1,23,5,27,1,9,27,31,1,31,10,35,2,35,9,39,1,5,39,43,2,43,9,47,1,5,47,51,2,51,13,55,1,55,10,59,1,59,10,63,2,9,63,67,1,67,5,71,2,13,71,75,1,75,10,79,1,79,6,83,2,13,83,87,1,87,6,91,1,6,91,95,1,10,95,99,2,99,6,103,1,103,5,107,2,6,107,111,1,10,111,115,1,115,5,119,2,6,119,123,1,123,5,127,2,127,6,131,1,131,5,135,1,2,135,139,1,139,13,0,99,2,0,14,0]

data Input = Input {noun :: Int, verb :: Int, mem :: MemState}
data Result = Result {noun :: Int, verb :: Int, mem :: MemState}

instance showResult :: Show Result where
  show (Result {noun, verb, mem}) = "Result: noun: " <> show noun <> " Verb: " <> show verb <> " memory: " <> show mem

partTwo = let progs = do
                n <- 0 .. 99
                v <- 0 .. 99
                pure $ Input {noun: n, verb: v, mem: 1 : n : v : program}
              results = map (\(Input {noun: n, verb: v, mem: m}) -> Result {noun: n, verb: v, mem: runProg m}) progs
          in filter (\(Result {noun, verb, mem}) -> head mem == Just 19690720) results
