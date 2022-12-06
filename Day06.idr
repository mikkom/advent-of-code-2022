module Day06

import AocUtils
import Data.Morphisms
import Data.SortedSet
import Data.String

%default total

-- Parsing

Input : Type
Input = List Char

parse : String -> Input
parse = unpack

-- Part 1

allDiffer : List Char -> Bool
allDiffer xs = length xs == length (SortedSet.toList (fromList xs))

solve1 : Input -> Maybe Nat
solve1 =
  map ((+ 4) . fst) . find (allDiffer . snd) . zipWithIndex . windowed' 4

part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

solve2 : Input -> Maybe Nat
solve2 =
  map ((+ 14) . fst) . find (allDiffer . snd) . zipWithIndex . windowed' 14

part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

covering
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
