module Day11

import AocUtils
import Data.List1
import Data.Maybe
import Data.Morphisms
import Data.Nat
import Data.SortedMap
import Data.Stream
import Data.String

%default total

-- Parsing

record Monkey where
  constructor MkMonkey
  items : List Int
  operation : (worry : Int) -> Int
  modulus : Nat
  ifTrue: Nat
  ifFalse: Nat

Input : Type
Input = SortedMap Nat Monkey

partial
parseMonkey : List String -> Monkey
parseMonkey (_ :: items :: op :: rest) =
  let [modulus, ifTrue, ifFalse] = map parseLast rest in
  MkMonkey (parseItems items) (parseOp $ words op) modulus ifTrue ifFalse
  where
    parseItems : String -> List Int
    parseItems =
      map (cast . trim) . forget . split (== ',') . snd . break isDigit

    parseOp : List String -> Int -> Int
    parseOp [_, _, _, arg1, op, arg2] x =
      (if op == "*" then (*) else (+))
        (if arg1 == "old" then x else cast arg1)
        (if arg2 == "old" then x else cast arg2)

    parseLast : String -> Nat
    parseLast = cast . last . ("0" :::) . words

partial
parse : String -> Input
parse =
  fromList . zipWithIndex . forget . map parseMonkey . split (== "") . lines

-- Part 1

State : Type
State = (Input, SortedMap Nat Int)

inspect : (Int -> Int) -> Monkey -> Input -> Int -> Input
inspect op (MkMonkey _ monkeyOp m ifTrue ifFalse) monkeys worry =
  fromMaybe monkeys $ do
    let worry' = op $ monkeyOp worry
    let targetNum = if mod worry' (cast m) == 0 then ifTrue else ifFalse
    target <- lookup targetNum monkeys
    let target' = { items $= (++ [worry']) } target
    pure $ insert targetNum target' monkeys

turn : (Int -> Int) -> State -> Nat -> State
turn op (monkeys, counts) i = fromMaybe (monkeys, counts) $ do
  monkey <- lookup i monkeys
  let monkeys' = foldl (inspect op monkey) monkeys monkey.items
  let monkey' = { items := [] } monkey
  let newCount = (fromMaybe 0 $ lookup i counts) + (cast $ length monkey.items)
  pure (insert i monkey' monkeys', insert i newCount counts)

round : (Int -> Int) -> State -> State
round op (monkeys, counts) =
  foldl (turn op) (monkeys, counts) (keys monkeys)

monkeyBusiness : Nat -> (Int -> Int) -> Input -> Int
monkeyBusiness n op =
  product . take 2 . reverse . sort . values . snd . index n .
    iterate (round op) . (, empty)

solve1 : Input -> Int
solve1 = monkeyBusiness 20 (`div` 3)

part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

partial
solve2 : Input -> Int
solve2 input =
  let modulus = foldl1 lcm $ 1 ::: map modulus (values input) in
  monkeyBusiness 10_000 (`mod` cast modulus) input
 
partial
part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

partial
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
