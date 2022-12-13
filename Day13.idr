module Day13

import AocUtils
import Data.List1
import Data.Morphisms
import Data.String

%default total

-- Parsing

data Packet = Single Int | Multi (List Packet)

Input : Type
Input = List (Packet, Packet)

mutual
  covering
  parsePacketList : List Char -> (List Packet, List Char)
  parsePacketList (']' :: cs) = ([], cs)
  parsePacketList cs =
    let cs = snd $ span (== ',') cs in
    let (p, cs) = parsePacket cs in
    let (ps, cs) = parsePacketList cs in
    (p :: ps, cs)

  covering
  parsePacket : List Char -> (Packet, List Char)
  parsePacket ('[' :: cs) =
    let (ps, cs') = parsePacketList cs in
    (Multi ps, cs')
  parsePacket cs =
    let (ds, cs) = span isDigit cs in
    (Single $ cast $ pack ds, cs)

partial
parse : String -> Input
parse = map (parsePair' . map (fst . parsePacket . unpack)) .
          forget . split (== "") . lines

-- Part 1

covering
Eq Packet where
  (==) (Single i) (Single j) = i == j
  (==) (Multi xs) (Multi ys) = xs == ys
  (==) _ _ = False

covering
Ord Packet where
  compare (Single i) (Single j) = compare i j
  compare (Multi []) (Multi []) = EQ
  compare (Multi []) (Multi (x :: xs)) = LT
  compare (Multi (x :: xs)) (Multi []) = GT
  compare (Multi (x :: xs)) (Multi (y :: ys)) =
    case compare x y of
      EQ => compare (Multi xs) (Multi ys)
      ord => ord
  compare m@(Multi xs) s@(Single i) = compare m (Multi [s])
  compare s@(Single i) m@(Multi xs) = compare (Multi [s]) m

covering
solve1 : Input -> Nat
solve1 = sum . map (S . fst) . filter (uncurry (<). snd) . zipWithIndex

covering
part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

dividers : List Packet
dividers = [Multi [Multi [Single 2]], Multi [Multi [Single 6]]]

covering
solve2 : Input -> Nat
solve2 = product . map (S . fst) . filter ((`elem` dividers) . snd) .
           zipWithIndex . sort . (dividers ++) . (>>= pairToList)

covering
part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

partial
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
