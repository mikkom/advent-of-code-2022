module Day05

import AocUtils
import Data.List1
import Data.Maybe
import Data.Morphisms
import Data.Nat
import Data.SortedMap
import Data.String

%default total

-- Parsing

Stack : Type
Stack = List Char

Stacks : Type
Stacks = SortedMap Nat Stack

record Move where
  constructor MkMove
  count : Nat
  from : Nat
  to : Nat

Input : Type
Input = (Stacks, List Move)

parseStackLine : Nat -> List Char -> List Char
parseStackLine count line =
  let indices = map (\i => 4 * pred i + 1) [1 .. count] in
  map (fromMaybe ' ' . safeIndex line) indices

update : List Stack -> String -> List Stack
update stacks line =
  let chars = parseStackLine (length stacks) (unpack line) in
  zipWith zipper chars stacks
  where
    zipper : Char -> Stack -> Stack
    zipper ' ' stack = stack
    zipper c stack = c :: stack

partial
parseStacks : List String -> Stacks
parseStacks ls =
  let (nums :: vals) = reverse ls in
  let count = length $ words nums in
  let stacks = foldl update (replicate count []) vals in
  fromList $ zipWithIndex stacks

partial
parseMove : List String -> Move
parseMove [_, count, _, from, _, to] =
  MkMove (cast count) (pred $ cast from) (pred $ cast to)

partial
parseMoves : List String -> List Move
parseMoves = map (parseMove . words)

partial
parse : String -> Input
parse = mapPair parseStacks parseMoves . parsePair . split (== "") . lines

-- Part 1

step : (Stack -> Stack -> Stack) -> Stacks -> Move -> Stacks
step stackFn stacks (MkMove count fromIdx toIdx) = fromMaybe stacks $ do
  from <- lookup fromIdx stacks
  to <- lookup toIdx stacks
  let (top, bottom) = splitAt count from
  let stacks' = insert fromIdx bottom stacks
  pure $ insert toIdx (stackFn top to) stacks'

stackCrates : (from : Stack) -> (to : Stack) -> Stack
stackCrates = flip $ foldl (flip (::))

topCrates : Stacks -> List Char
topCrates = catMaybes . map head' . values

solve1 : Input -> String
solve1 (stacks, moves) =
  pack $ topCrates $ foldl (step stackCrates) stacks moves

part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

solve2 : Input -> String
solve2 (stacks, moves) = pack $ topCrates $ foldl (step (++)) stacks moves

part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

partial
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
