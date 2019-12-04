module Four where

import Prelude

import Data.Array (all, any, drop, filter, length, take, zip, (..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

import Partial.Unsafe (unsafePartial)

validPassword :: Int -> Boolean
validPassword pi = let str = toCharArray (show pi)
                   in increasing str && hasDouble str

increasing :: Array Char -> Boolean
increasing cs = let as = take 5 cs
                    bs = drop 1 cs
                    asbs = zip as bs
                in all (\(Tuple x y) -> compare x y /= GT) asbs

hasDouble :: Array Char -> Boolean
hasDouble cs = let as = take 5 cs
                   bs = drop 1 cs
                   asbs = zip as bs
               in any (\(Tuple x y) -> compare x y == EQ) asbs

partOne :: Int -> Int -> Int
partOne s e = length $ filter validPassword (s .. e)

-- Brute forcing it
hasDouble' :: Partial => Array Char -> Boolean
hasDouble' [a, b, c, d, e, f] = (a == b && c /= b)
                                || (b == c && a /= b && c /= d)
                                || (c == d && b /= c && d /= e)
                                || (d == e && c /= d && e /= f)
                                || (e == f && d /= e)

validPassword2 :: Int -> Boolean
validPassword2 pi = let str = toCharArray (show pi)
                    in increasing str && (unsafePartial $ hasDouble' str)

partTwo :: Int -> Int -> Int
partTwo s e = length $ filter validPassword2 (s .. e)
