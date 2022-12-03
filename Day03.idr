module Day03

import AocUtils
import Data.List1
import Data.Morphisms
import Data.Nat
import Data.SortedSet
import Data.String

%default total

-- Parsing

Rucksack : Type
Rucksack = List Char

Input : Type
Input = List Rucksack

parse : String -> Input
parse = map unpack . lines

-- Part 1

priority : Char -> Int
priority ch with (isUpper ch)
  priority ch | False = ord ch - ord 'a' + 1
  priority ch | True = ord ch - ord 'A' + 27

partial
findShared : Rucksack -> Maybe Char
findShared = head' . SortedSet.toList . uncurry intersection . compartments
  where
    compartments : Rucksack -> (SortedSet Char, SortedSet Char)
    compartments sack = mapBoth fromList $ splitAt (length sack `div` 2) sack

solve1 : Input -> Maybe Int
solve1 = map (sum . map priority) . traverse findShared

part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

findBadge : List1 Rucksack -> Maybe Char
findBadge = head' . SortedSet.toList . foldl1 intersection . map fromList

partial
solve2 : Input -> Maybe Int
solve2 = map (sum . map priority) . traverse findBadge . chunksOf 3

partial
part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

covering
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
