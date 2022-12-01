module Day01

import AocUtils
import Data.List1
import Data.Morphisms
import Data.String

%default total

-- Parsing

Input : Type
Input = List1 (List Nat)

toNat : String -> Nat
toNat = cast

parse : String -> Input
parse = map (map toNat) . split (== "") . lines

-- Part 1

solve1 : Input -> Nat
solve1 = foldl1 max . map sum

part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

solve2 : Input -> Nat
solve2 = sum . take 3 . reverse . sort . map sum . forget

part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

covering
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
