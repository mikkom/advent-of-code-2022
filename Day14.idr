module Day14

import AocUtils
import Data.List1
import Data.Maybe
import Data.Nat
import Data.Morphisms
import Data.SortedSet
import Data.String

%default total

-- Parsing

Coord : Type
Coord = (Int, Int)

Input : Type
Input = SortedSet Coord

%hint
additive : Num a => Monoid a
additive = Additive

covering
createLine : Coord -> Coord -> List Coord
createLine c@(x, y) c'@(x', y') =
  let delta = (signum $ x' - x, signum $ y' - y) in
  c :: unfoldr (until c' (<+> delta)) c
  where
    until : Coord -> (Coord -> Coord) -> Coord -> Maybe (Coord, Coord)
    until last next c = if c == last then Nothing else Just (next c, next c)

covering
createPath : List Coord -> List Coord
createPath (c :: c' :: cs) = createLine c c' ++ createPath (c' :: cs)
createPath xs = xs

partial
parsePath : List String -> List Coord
parsePath =
  createPath . map (parsePair . map cast . split (== ',')) . filter (/= "->")

partial
parse : String -> Input
parse =  fromList . concatMap (parsePath . words) . lines

-- Part 1

sandStart : Coord
sandStart = (500, 0)

deltas : List Coord
deltas = [(0, 1), (-1, 1), (1, 1)]

move : Coord -> Input -> Maybe Coord
move c cave =
  let cs = map (c <+>) deltas in
  case zip cs $ map (`contains` cave) cs of
    [(c, False), _, _] => Just c
    [_, (c, False), _] => Just c
    [_, _, (c, False)] => Just c
    _ => Nothing

Step : Type
Step = Int -> Coord -> Input -> Maybe (Coord, Input)

covering
step : Step
step maxDepth c@(x, y) cave =
  if y >= maxDepth
  then Nothing
  else
    case move c cave of
      Just c => step maxDepth c cave
      Nothing => Just (sandStart, insert c cave)

covering
countSand : Step -> Input -> Nat
countSand step input =
  let maxDepth = max1 $ (0 :::) $ map snd $ SortedSet.toList input in
  pred $ length $ iterate (uncurry $ step maxDepth) $ (sandStart, input)

covering
solve1 : Input -> Nat
solve1 = countSand step

covering
part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

covering
step' : Step
step' maxDepth c@(x, y) cave =
  if contains c cave
  then Nothing
  else if y == maxDepth + 1
  then Just (sandStart, insert c cave)
  else
    case move c cave of
      Just c => step' maxDepth c cave
      Nothing => Just (sandStart, insert c cave)

covering
solve2 : Input -> Nat
solve2 = countSand step'

covering
part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

partial
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
