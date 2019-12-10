module IntcodeBoost where

import Data.Array
import Data.BigInt (BigInt(..), fromInt, fromString, toString)
import Data.Int (fromString) as I
import Data.Maybe
import Data.Show
import Data.Tuple
import Debug.Trace
import Debug.Trace
import Prelude

import Data.String (length, split, splitAt) as Str
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)

type MemState
  = Array BigInt

type IP
  = Int

type RB
  = Int

type Input
  = Array BigInt

type Output
  = Array BigInt

data Status
  = Running
  | Halted
  | Paused

instance statusShow :: Show Status where
  show Running = "Running"
  show Halted = "Halted"
  show Paused = "Paused"

data ProgState
  = ProgState MemState IP RB Status Input Output

instance progstateshow :: Show ProgState where
  show (ProgState prog pos rb status input output) =
    "ProgState "
      <> show prog
      <> " "
      <> show pos
      <> " "
      <> show status
      <> " "
      <> show input
      <> " "
      <> show output

data Result
  = Result { noun :: BigInt, verb :: BigInt, mem :: MemState }

instance showResult :: Show Result where
  show (Result { noun, verb, mem }) =
    "Result: noun: " <> show noun
      <> " Verb: "
      <> show verb
      <> " memory: "
      <> show mem

newtype Opcode
  = Opcode Int

instance showOpcode :: Show Opcode where
  show (Opcode op) = "Opcode " <> show op

data OpMode
  = OpMode Opcode (Array Char)

instance showOpMode :: Show OpMode where
  show (OpMode op params) = "OpCode " <> show op <> " " <> show params

reverseString :: String -> String
reverseString s = fromCharArray $ reverse $ toCharArray s

parseOpMode :: BigInt -> OpMode
parseOpMode i =
  let
    as = reverseString $ show (toInt i)

    { before: ops, after: params } = Str.splitAt 2 as

    op = case I.fromString $ reverseString ops of
      Just o -> o
      Nothing -> spy ("error parsing op " <> ops) 0
  in
    OpMode (Opcode op) (toCharArray params)

opInsLength :: Partial => Opcode -> Int
opInsLength (Opcode 1) = 4 -- add
opInsLength (Opcode 2) = 4 -- multiply
opInsLength (Opcode 3) = 2 -- input
opInsLength (Opcode 4) = 2 -- output
opInsLength (Opcode 5) = 3 -- JT/JNZ
opInsLength (Opcode 6) = 3 -- JF/JZ
opInsLength (Opcode 7) = 4 -- LT
opInsLength (Opcode 8) = 4 -- EQ
opInsLength (Opcode 9) = 2 -- set relative base
opInsLength (Opcode 99) = 1 -- halt
opInsLength (Opcode x) = spy "invalid opcode:" x


toInt :: BigInt -> Int
toInt bi = case I.fromString $ toString bi of
  Just i -> i
  Nothing -> spy ("error converting bigint " <> toString bi <> " to int") $ 9999

getVals :: MemState -> RB -> Array BigInt -> Array Char -> Maybe (Array BigInt)
getVals mem rb ps pms = traverse (\(Tuple p i) -> case index pms i of
                                     Just '1' -> Just p
                                     Just '2' -> index mem (rb + (toInt p))
                                     _ -> index mem (toInt p))
                        (zip ps (0..(length ps)))


step :: Partial => ProgState -> ProgState
step ps@(ProgState _ _ _ Halted _ _) = ps
step (ProgState mem pos rb _ input output) =
  let
    (OpMode op modes) = parseOpMode (fromMaybe (fromInt 0) (index mem pos))

    insLength = opInsLength op

    instruction = slice pos (pos + insLength) mem
  in case insLength of
    4 ->
      let
        [ _, a, b, dst ] = instruction

        Just [aval, bval] = getVals mem rb [a, b] modes

        val = case op of
          Opcode 1 -> aval + bval
          Opcode 2 -> aval * bval
          Opcode 7 -> if aval < bval then (fromInt 1) else (fromInt 0)
          Opcode 8 -> if aval == bval then (fromInt 1) else (fromInt 0)
        dst' = case index modes 2 of
          Just '2' -> rb + (toInt dst)
          _ -> toInt dst
        Just newstate = modifyAt dst' (const val) mem
      in
       ProgState newstate (pos + insLength) rb Running input output
    3 ->
      let
        [ _, a, goto] = instruction
        Just [aval, gotoval] = getVals mem rb [a, goto] modes
        jump = case op of
          Opcode 5 -> aval /= (fromInt 0)
          Opcode 6 -> aval == (fromInt 0)
        newpc = if jump then (toInt gotoval) else pos + insLength
      in
       ProgState mem newpc rb Running input output
    2 ->
      let
        [ _, dst ] = instruction
      in
       case op of
         Opcode 3 ->
           let
             Just inval = head input
             dst' = case head modes of
               Just '2' -> rb + toInt dst
               _ -> toInt dst
             Just newstate = modifyAt dst' (const inval) mem
           in
            (ProgState
             newstate
             (pos + insLength)
             rb
             Running
             (fromMaybe [] $ tail input)
             output)
         Opcode 4 ->
           let Just [val] = getVals mem rb [dst] modes
           in ProgState mem (pos + insLength) rb Paused input (snoc output val)
         Opcode 9 ->
           let
             Just [dst'] = getVals mem rb [dst] modes
           in
            ProgState mem (pos + insLength) (rb + toInt dst')
            Running input output
    1 -> case op of
      Opcode 99 -> ProgState mem (pos + insLength) rb Halted input output
    _ -> ProgState mem (pos + insLength) rb Halted input output

runProg :: Input -> MemState -> ProgState
runProg input p = runProg' (newProgState input p)
  where
  runProg' ps@(ProgState _ _ _ Halted _ _) = ps
  runProg' ps = runProg' $ unsafePartial step ps

newProgState :: Input -> MemState -> ProgState
newProgState input p =
  (ProgState (p <> replicate (65536 - (length p)) (fromInt 0))
   0 0 Running input [])

runProgState :: ProgState -> ProgState
runProgState ps@(ProgState _ _ _ Halted _ _) = ps
runProgState ps@(ProgState _ _ _ _ _ _) =
  runProgState' $ unsafePartial step ps
  where
    runProgState' ps'@(ProgState _ _ _ Running _ _) =
      runProgState' $ unsafePartial step ps'
    runProgState' ps' = ps'

appendInput :: BigInt -> ProgState -> ProgState
appendInput i (ProgState ms ip rb status input output) =
  ProgState ms ip rb status (snoc input i) output

runOutput :: Input -> MemState -> Array BigInt
runOutput input p = unsafePartial case runProg input p of
  ProgState _ _ _ _ _ x -> x

parseInput :: String -> Maybe MemState
parseInput s = traverse fromString (Str.split (Pattern ",") s)
