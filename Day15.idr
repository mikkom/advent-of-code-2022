module Day15

import AocUtils
import Interval
import Data.List1
import Data.Morphisms
import Data.String

%default total

-- Parsing

Coord : Type
Coord = (Int, Int)

record Sensor where
  constructor MkSensor
  position : Coord
  beacon : Coord

Input : Type
Input = List Sensor

isNumChar : Char -> Bool
isNumChar '-' = True
isNumChar ch = isDigit ch

partial
parseCoord : List1 String -> Coord
parseCoord = parsePair . map (cast . snd . break isNumChar)

partial
parseSensor : String -> Sensor
parseSensor = uncurry MkSensor . parsePair .
                map (parseCoord . split (== ',')) . split (== ':')

partial
parse : String -> Input
parse = map parseSensor . lines

-- Part 1

getInterval : Int -> Sensor -> Intervals
getInterval y (MkSensor (sx, sy) (bx, by)) =
  let radius = abs (bx - sx) + abs (by - sy) in
  let distance = abs (y - sy) in
  let intervalRadius = max 0 $ radius - distance + 1 in
  if intervalRadius == 0 then empty
  else singleton $ MkInterval (sx - intervalRadius + 1) (sx + intervalRadius)

coverage : Int -> Input -> Intervals
coverage y = foldMap (getInterval y)

coverageWithoutBeacons : Int -> Input -> Intervals
coverageWithoutBeacons y input =
  let cover = coverage y input in
  let bxs = map fst $ filter ((== y) . snd) $ map beacon input in
  foldl (flip remove) cover bxs

solve1 : Input -> Nat
solve1 = sum . map length . intervals . coverageWithoutBeacons 2_000_000

part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

findMissing : (searchArea : Interval) -> (cover : List Interval) -> Maybe Int
findMissing (MkInterval from until) [] = Just from
findMissing (MkInterval from until) [i] =
  if i.from > from then Just from
  else if i.until < until then Just i.until
  else Nothing
findMissing (MkInterval from until) (i :: is) =
  if i.from > from then Just from
  else findMissing (MkInterval i.until until) is 

maxCoord : Int
maxCoord = 4_000_000

findBeacon : Int -> Input -> Maybe Coord
findBeacon y =
  map (, y) . findMissing (MkInterval 0 (maxCoord + 1)) . intervals . coverage y

freq : Coord -> Int
freq (x, y) = maxCoord * x + y

solve2 : Input -> Maybe Int
-- fixme: add short circuiting
solve2 input =
  map freq $ head' $ catMaybes $ map (flip findBeacon input) [0 .. maxCoord]

part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

partial
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
