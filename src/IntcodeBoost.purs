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

type Input
  = Array BigInt

type Output
  = Maybe BigInt

data Status
  = Running
  | Halted
  | Paused

instance statusShow :: Show Status where
  show Running = "Running"
  show Halted = "Halted"
  show Paused = "Paused"

data ProgState
  = ProgState MemState IP Status Input Output

instance progstateshow :: Show ProgState where
  show (ProgState prog pos status input output) =
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

    op = fromMaybe 0 $ I.fromString $ reverseString ops
  in
    spy "op:" $ OpMode (Opcode op) (toCharArray params)

opInsLength :: Partial => Opcode -> Int
opInsLength (Opcode 1) = 4 -- add
opInsLength (Opcode 2) = 4 -- multiply
opInsLength (Opcode 3) = 2 -- input
opInsLength (Opcode 4) = 2 -- output
opInsLength (Opcode 5) = 3 -- JT/JNZ
opInsLength (Opcode 6) = 3 -- JF/JZ
opInsLength (Opcode 7) = 4 -- LT
opInsLength (Opcode 8) = 4 -- EQ
opInsLength (Opcode 99) = 1 -- Halt
opInsLength (Opcode x) = spy "invalid opcode:" x


toInt :: BigInt -> Int
toInt bi = fromMaybe 0 $ I.fromString $ toString bi

getVals :: MemState -> Array BigInt -> Array Char -> Array (Maybe BigInt)
getVals mem ps pms = map (\(Tuple p i) -> case index pms i of
                               Just '1' -> Just p
                               _ -> index mem (toInt p))
                     (zip ps (0..(length ps)))


step :: Partial => ProgState -> ProgState
step ps@(ProgState _ _ Halted _ _) = ps
step (ProgState prog pos _ input output) =
  let
    (OpMode op modes) = parseOpMode (fromMaybe (fromInt 0) (index prog pos))

    insLength = opInsLength op

    instruction = slice pos (pos + insLength) prog
  in case insLength of
    4 ->
      let
        [ _, a, b, dst ] = instruction

        [Just aval, Just bval] = getVals prog [a, b] modes

        val = case op of
          Opcode 1 -> aval + bval
          Opcode 2 -> aval * bval
          Opcode 7 -> if aval < bval then (fromInt 1) else (fromInt 0)
          Opcode 8 -> if aval == bval then (fromInt 1) else (fromInt 0)
        Just newstate = modifyAt (toInt dst) (const val) prog
      in
       ProgState newstate (pos + insLength) Running input output
    3 ->
      let
        [ _, a, goto] = instruction
        [Just aval, Just gotoval] = getVals prog [a, goto] modes
        jump = case op of
          Opcode 5 -> aval /= (fromInt 0)
          Opcode 6 -> aval == (fromInt 0)
        newpc = if jump then (toInt gotoval) else pos + insLength
      in
       ProgState prog newpc Running input output
    2 ->
      let
        [ _, dst ] = instruction
      in
       case op of
         Opcode 3 ->
           let
             Just inval = head input
             Just newstate = modifyAt (toInt dst) (const inval) prog
           in
            (ProgState
             newstate
             (pos + insLength)
             Running
             (fromMaybe [] $ tail input)
             output)
         Opcode 4 ->
           let
             [val] = getVals prog [dst] modes
           in
            ProgState prog (pos + insLength) Paused input val
    1 -> case op of
      Opcode 99 -> ProgState prog (pos + insLength) Halted input output
    _ -> ProgState prog (pos + insLength) Halted input output

runProg :: Input -> MemState -> ProgState
runProg input p = runProg' (newProgState input p)
  where
  runProg' ps@(ProgState _ _ Halted _ _) = ps
  runProg' ps@(ProgState _ _ Paused _ _) = ps
  runProg' ps = runProg' $ spy "ps:" $ unsafePartial step ps

newProgState :: Input -> MemState -> ProgState
newProgState input p = (ProgState p 0 Running input Nothing)

runProgState :: ProgState -> ProgState
runProgState ps@(ProgState _ _ Halted _ _) = ps
runProgState ps@(ProgState _ _ _ _ _) =
  runProgState' $ unsafePartial step ps
  where
    runProgState' ps'@(ProgState _ _ Running _ _) =
      runProgState' $ unsafePartial step ps'
    runProgState' ps' = ps'

appendInput :: BigInt -> ProgState -> ProgState
appendInput i (ProgState ms ip status input output) =
  ProgState ms ip status (snoc input i) output

runOutput :: Input -> MemState -> BigInt
runOutput input p = unsafePartial case runProg input p of
  ProgState _ _ _ _ (Just x) -> x

parseInput :: String -> Maybe MemState
parseInput s = traverse fromString (Str.split (Pattern ",") s)
