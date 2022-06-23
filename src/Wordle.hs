{-# LANGUAGE EmptyDataDecls #-}
module Wordle where

import Data.Maybe
import Interact
import Data.Char
import Data.List

{- | 
任意の入力列をそれぞれの入力文字列を"なんか関数"に変換
>>> putStr $ unlines $ responser undefined ["Hi.", "お元気ですか？"]
なんか関数
なんか関数
-}
{-
data Maybe a
    = Nothing
    | Just a
-}
wordle :: String -> ([String] -> [String])
wordle extra = mapMaybe output . eval . initial extra

data MachineState
    = MachineState 
    { inChan :: [String]
    , output :: Maybe String
    , innerState :: InnerState
    }

type Dict = [String]
type InnerState = Dict

initial :: String -> [String] -> MachineState
initial extra inputs
    = MachineState
    { inChan = inputs
    , output = Nothing
    , innerState = initialDict (words extra)
    }

initialDict :: Dict -> Dict
initialDict = filter (all isAscii) . filter (all isLower) . filter ((5 ==) . length)

eval :: MachineState -> [MachineState]
eval state = state : rests
    where
        rests | isFinal state = []
              | otherwise     = eval (step state)

isFinal :: MachineState -> Bool
isFinal state = case state of
    MachineState { inChan = [] } -> True
    _                            -> False

step :: MachineState -> MachineState
step state = case state of
    MachineState { inChan = i : is
                 , innerState = olddict
                 } -> state { inChan = is
                            , innerState = newdict 
                            , output = Just (unlines newdict)
                            }
        where
            newdict = filter f olddict
            f = predicate guess pattern
            [guess, pattern] = words i

type Guess = String
type Pattern = String
type Candidate  = String

predicate :: Guess -> Pattern -> (Candidate -> Bool)
predicate guess pattern candidate 
    = pattern == makePattern guess candidate

makePattern :: Guess -> Candidate -> Pattern
makePattern guess candidate
    = case unzip (zipWith proc1 guess candidate) of
        (guess', candidate') -> case mapAccumL proc2 candidate' guess' of
            (candidate'', guess'') -> guess''
    where
        proc1 c d | c == d    = ('-', '-')
                  | otherwise = ( c , d)
        proc2 ds c 
            | c == '-' = (ds, c)
            | otherwise = case break (c ==) ds of
            (xs, [])   -> (xs, '*')
            (xs, y:ys) -> (xs ++ '+':ys, '+')

{-
mapAccumL :: (acc -> a -> (acc, b)) -> acc -> [a] -> (acc, [b])
mapAccumL f acc0 xs = case xs of
    []   -> (acc0, [])
    y:ys -> case f acc0 y of
        (acc1, z) -> case mapAccumL f acc1 ys of
            (acc, zs) -> (acc, z:zs)

    mapAccumL proc2 "gl-om" "mo-dy"
=   { mapAccumL と proc2  の定義 }
    proc2 "gl-om" 'm' = ("gl-o+", '+') なので
    case mapAccumL proc2 "gl-o+" "o-dy" of
        (acc, zs) -> (acc, '+':zs)

       [a1,  a2, .. , an]
        |     |        |
acc0 -> f  -> f -> ->  f  -> accn
        |     |        |
        b1    b2       bn
-}