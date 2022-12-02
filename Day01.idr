module Day01

import AocUtils
import Data.List1
import Data.Morphisms
import Data.String
import Data.Vect

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

solve2' : Input -> Nat
solve2' = sum . foldl update [0, 0, 0] . map sum
  where
    update : List Nat -> Nat -> List Nat
    update xs x = safeTail $ sort (x :: xs)
    

solve2'' : Input -> Nat
solve2'' = sum . foldl update [0, 0, 0] . map sum
  where
    insert : Nat -> Vect 2 Nat -> Vect 3 Nat
    insert x [y, z] =
      if x <= y then [x, y, z]
      else if x <= z then [y, x, z]
      else [y, z, x]

    update : Vect 3 Nat -> Nat -> Vect 3 Nat
    update (y :: ys) x = if x > y then insert x ys else (y :: ys)

part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2''

-- Plumbing

covering
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
