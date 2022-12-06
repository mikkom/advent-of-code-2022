module Day04

import AocUtils
import Data.List1
import Data.Morphisms
import Data.String

%default total

-- Parsing

Interval : Type
Interval = (Nat, Nat)

Input : Type
Input = List (Interval, Interval)

partial
parseInterval : String -> Interval
parseInterval = parsePair . map cast . split (== '-')

partial
parse : String -> Input
parse = map (parsePair . map parseInterval . split (== ',')) . lines

-- Part 1

fullyContained : (Interval, Interval) -> Bool
fullyContained pair = check pair || (check $ swap pair)
  where
    check : (Interval, Interval) -> Bool
    check ((from, to), (from', to')) = from >= from' && to <= to'

solve1 : Input -> Nat
solve1 = count fullyContained

part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

overlap : (Interval, Interval) -> Bool
overlap pair = check pair || (check $ swap pair)
  where
    check : (Interval, Interval) -> Bool
    check ((from, to), (from', to')) = to >= from' && to <= to'

solve2 : Input -> Nat
solve2 = count overlap

part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

partial
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
