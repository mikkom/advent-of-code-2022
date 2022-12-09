module Day09

import AocUtils
import Data.List1
import Data.Morphisms
import Data.Nat
import Data.SortedSet
import Data.String

%default total

-- Parsing

Input : Type
Input = List (String, Nat)

partial
parse : String -> Input
parse = map (parseLine . words) . lines
  where
    parseLine : List String -> (String, Nat)
    parseLine [dir, cnt] = (dir, cast cnt)

-- Part 1

%hint
additive : Num a => Monoid a
additive = Additive

Coord : Type
Coord = (Int, Int)

Rope : Type
Rope = List1 Coord

followKnot : (knot : Coord) -> (next: Coord) -> Coord
followKnot (hx, hy) (tx, ty) =
  let (dx, dy) = (hx - tx, hy - ty) in
  if abs dx <= 1 && abs dy <= 1
  then (tx, ty) 
  else (tx, ty) <+> mapBoth signum (dx, dy)

getDelta : String -> Coord
getDelta "L" = (-1, 0)
getDelta "R" = (1, 0)
getDelta "U" = (0, 1)
getDelta _ = (0, -1)

covering
step : (Rope, Input) -> Maybe (Rope, Input)
step (rope, []) = Nothing
step (rope, ((dir, 0) :: xs)) = step (rope, xs)
step ((hd ::: tl), ((dir, n) :: xs)) =
  let hd' = hd <+> getDelta dir in
  Just (knotStep hd' tl, (dir, pred n) :: xs)
  where
    knotStep : Coord -> List Coord -> List1 Coord
    knotStep knot [] = singleton knot
    knotStep knot (next :: rest) =
      knot ::: (toList $ knotStep (followKnot knot next) rest)

covering
moveRope : (len : Nat) -> NonZero len => Input -> List Rope
moveRope (S k) = map fst . iterate step . ((0, 0) ::: replicate k (0, 0),)

covering
solve1 : Input -> Nat
solve1 = length . nub . map last . moveRope 2

covering
part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

covering
solve2 : Input -> Nat
solve2 = length . nub . map last . moveRope 10

covering
part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

partial
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
