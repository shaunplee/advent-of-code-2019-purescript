module Seven where

import Data.Array
import Data.Maybe
import Data.Tuple
import Intcode
import Partial.Unsafe
import Prelude

import Data.Foldable (maximumBy)

testSequence :: Partial => MemState -> Array Int -> Int
testSequence prog [pA, pB, pC, pD, pE] =
  let
    outputA = runOutput [pA, 0] prog
    outputB = runOutput [pB, outputA] prog
    outputC = runOutput [pC, outputB] prog
    outputD = runOutput [pD, outputC] prog
    outputE = runOutput [pE, outputD] prog
  in
   outputE

permutations :: forall a. Eq a => Array a -> Array (Array a)
permutations [] = []
permutations xsi
  | length xsi == 1 = [xsi]
  | otherwise = let newArrays = map (\x -> Tuple x (delete x xsi)) xsi
                in concatMap (\(Tuple x xs) ->
                               map (flip snoc x) (permutations xs))
                   newArrays

findBestSequence :: MemState -> Maybe (Tuple (Array Int) Int)
findBestSequence prog =
  maximumBy (\(Tuple seq1 score1) (Tuple seq2 score2) -> compare score1 score2)
  (map (\seq -> Tuple seq (unsafePartial $ testSequence prog seq))
   (permutations (0..4) ))

partOne :: Int
partOne = case findBestSequence input of
  Just (Tuple seq score) -> score
  Nothing -> 0

data AmpState = AmpState ProgState ProgState ProgState ProgState ProgState

testFeedbackSequence :: Partial => MemState -> Array Int -> Int
testFeedbackSequence prog [pA, pB, pC, pD, pE] =
  let
    initA = newProgState [pA, 0] prog
    initB = newProgState [pB] prog
    initC = newProgState [pC] prog
    initD = newProgState [pD] prog
    initE = newProgState [pE] prog
  in oneLoop (AmpState initA initB initC initD initE)
  where
    oneLoop :: AmpState -> Int
    oneLoop (AmpState psa psb psc psd pse) =
      let
        npsa@(ProgState _ _ _ _ (Just outputA)) =
          runProgState psa
        npsb@(ProgState _ _ _ _ (Just outputB)) =
          runProgState (appendInput outputA psb)
        npsc@(ProgState _ _ _ _ (Just outputC)) =
          runProgState (appendInput outputB psc)
        npsd@(ProgState _ _ _ _ (Just outputD)) =
          runProgState (appendInput outputC psd)
        npse@(ProgState _ _ statusE _ (Just outputE)) =
          runProgState (appendInput outputD pse)
      in case statusE of
        Halted -> outputE
        _ -> oneLoop $ AmpState (appendInput outputE npsa) npsb npsc npsd npse

findBestFeedbackSequnce :: MemState -> Maybe (Tuple (Array Int) Int)
findBestFeedbackSequnce initProg =
  maximumBy (\(Tuple seq1 score1) (Tuple seq2 score2) -> compare score1 score2)
  (map (\seq -> Tuple seq (unsafePartial $ testFeedbackSequence initProg seq))
   (permutations (5..9) ))

partTwo :: Int
partTwo = case findBestFeedbackSequnce input of
  Just (Tuple seq score) -> score
  Nothing -> 0

testInput1 :: MemState
testInput1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]

testInput2 :: MemState
testInput2  = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]

testInput3 :: MemState
testInput3  = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]

input :: MemState
input = [3,8,1001,8,10,8,105,1,0,0,21,42,67,84,109,126,207,288,369,450,99999,3,9,102,4,9,9,1001,9,4,9,102,2,9,9,101,2,9,9,4,9,99,3,9,1001,9,5,9,1002,9,5,9,1001,9,5,9,1002,9,5,9,101,5,9,9,4,9,99,3,9,101,5,9,9,1002,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,2,9,102,4,9,9,101,2,9,9,102,4,9,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,101,5,9,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,99]
