module Day08

import AocUtils
import Data.List1
import Data.Nat
import Data.Maybe
import Data.Morphisms
import Data.SortedMap
import Data.String

%default total

-- Parsing

Coord : Type
Coord = (Int, Int)

Input : Type
Input = SortedMap Coord Nat

parse : String -> Input
parse = fromList . (>>= parseLine) . zipWithIndex . lines
  where
    formPair : Nat -> (Nat, Char) -> (Coord, Nat)
    formPair i (j, ch) = ((cast i, cast j), cast $ String.singleton ch)

    parseLine : (Nat, String) -> List (Coord, Nat)
    parseLine (i, str) = map (formPair i) $ zipWithIndex $ unpack str
  
-- Part 1

%hint
additive : Num a => Monoid a
additive = Additive

dirs : List Coord
dirs = [(0, -1), (0, 1), (-1, 0), (1, 0)] 

covering
visible : Input -> Coord -> Bool
visible treeMap pos =
  case lookup pos treeMap of
    Nothing => False
    Just height => any (visible' height pos) dirs
  where
    visible' : (height : Nat) -> (pos : Coord) -> (dir : Coord) -> Bool
    visible' height pos dir =
      let pos' = pos <+> dir in
      case lookup pos' treeMap of
        Nothing => True
        Just h => height > h && visible' height pos' dir

covering
solve1 : Input -> Nat
solve1 treeMap = length $ filter (visible treeMap) $ keys treeMap

covering
part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

covering
viewingDistances : Input -> Coord -> List Nat
viewingDistances treeMap pos =
  case lookup pos treeMap of
    Nothing => []
    Just height => map (distance 0 height pos) dirs
  where
    distance : Nat -> (height : Nat) -> (pos : Coord) -> (dir : Coord) -> Nat
    distance acc height pos dir =
      let pos' = pos <+> dir in
      case lookup pos' treeMap of
        Nothing => acc
        Just h =>
          if h >= height
          then S acc
          else distance (S acc) height pos' dir

covering
solve2 : Input -> Nat
solve2 treeMap =
  max' $ (0 :::) $ map (product . viewingDistances treeMap) $ keys treeMap

covering
part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

covering
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
