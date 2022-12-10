module Day10

import AocUtils
import Data.List1
import Data.Morphisms
import Data.String
import Data.Vect

%default total

-- Parsing

data Cmd = Addx Int | Noop

Input : Type
Input = List Cmd

parseCmd : List String -> Cmd
parseCmd ["addx", val] = Addx (cast val)
parseCmd _ = Noop

parse : String -> Input
parse = map (parseCmd . words) . lines

-- Part 1

cmdEffect : Cmd -> List Int
cmdEffect (Addx i) = [0, i]
cmdEffect Noop = [0]

registerValues : Input -> List (Nat, Int)
registerValues input =
  zipWithIndex $ toList $ scanl (+) 1 $ fromList $ (>>= cmdEffect) input

covering
interesting : List a -> List a
interesting xs =
  let (xs, ys) = splitAt 1 $ drop (pred 20) xs in
  xs ++ everyNth 40 ys

signalStrength : Nat -> Int -> Int
signalStrength idx val = cast (S idx) * val

covering
solve1 : Input -> Int
solve1 = sum . map (uncurry signalStrength) . interesting . registerValues

covering
part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

isLit : (idx : Nat) -> (x : Int) -> Bool
isLit idx x = cast (mod idx 40) `elem` [x - 1 .. x + 1]

covering
drawScreen : List Bool -> String
drawScreen = unlines . map (pack . forget) . chunksOf 40 .
    map (\b => if b then '#' else '.')

covering
solve2 : Input -> String
solve2 = drawScreen . map (uncurry isLit) . registerValues

covering
part2 : Input ~> String
part2 = Mor $ ("Part 2: \n" ++) . solve2

-- Plumbing

covering
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
