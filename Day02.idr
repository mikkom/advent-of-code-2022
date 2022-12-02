module Day02

import AocUtils
import Data.Morphisms
import Data.String

%default total

-- Parsing

Match : Type
Match = (Int, Int)

Input : Type
Input = List Match

parseMatch : String -> Match
parseMatch = parse . unpack
  where
    parse : List Char -> Match
    parse [f, _, s] = (cast f - cast 'A', cast s - cast 'X')
    parse _ = (0, 0)

parse : String -> Input
parse = map parseMatch . lines

-- Part 1

score: Match -> Int
score (their, mine) = mine + 1 +
  case mod (mine - their) 3 of
    0 => 3 -- draw
    1 => 6 -- win
    _ => 0 -- loss

%hint
additive : Num a => Monoid a
additive = Additive

solve1 : Input -> Int 
solve1 = foldMap score

part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

score2 : Match -> Int
score2 (their, result) = 3 * result + 1 +
  case result of
    0 => mod (their + 2) 3 -- loss
    1 => their -- draw
    _ => mod (their + 1) 3 -- win

solve2 : Input -> Int
solve2 = foldMap score2

part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

covering
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
