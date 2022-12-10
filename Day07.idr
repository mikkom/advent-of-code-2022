module Day07

import AocUtils
import Data.List1
import Data.Maybe
import Data.Morphisms
import Data.SortedMap
import Data.String

%default total

-- Parsing

data DirOutput
  = DirLine String
  | FileLine String Nat

data CmdOutput
  = CdRoot
  | CdParent
  | Cd String
  | Ls (List DirOutput)

Show DirOutput where
  show (DirLine str) = "dir \{str}"
  show (FileLine str k) = "\{show k} \{str}"

Show CmdOutput where
  show CdRoot = "cd /"
  show CdParent = "cd .."
  show (Cd str) = "cd \{str}"
  show (Ls output) = "ls \n\{unlines $ map show output}"

Input : Type
Input = List CmdOutput

partial
parseCmdOutput : List String -> CmdOutput
parseCmdOutput ["cd /"] = CdRoot
parseCmdOutput ["cd .."] = CdParent
parseCmdOutput ("ls" :: output) = Ls (map (parseDirOutput . words) output)
  where
    parseDirOutput : List String -> DirOutput
    parseDirOutput ["dir", name] = DirLine name
    parseDirOutput [size, name] = FileLine name (cast size)
parseCmdOutput [cdCmd] = parseCd $ words cdCmd
  where
    parseCd : List String -> CmdOutput
    parseCd ["cd", dirName] = Cd dirName

partial
parse : String -> Input
parse = map parseCmdOutput . map (lines . trim) . tail . split (== '$')

-- Part 1

record DirTree where
  constructor MkDirTree
  dirs : SortedMap String DirTree
  files : List (String, Nat)

emptyDir : DirTree
emptyDir = MkDirTree empty []

covering
formTree : (acc : DirTree) -> Input -> (DirTree, Input)
formTree acc [] = (acc, [])
formTree acc (CdRoot :: rest) = formTree emptyDir rest 
formTree acc (CdParent :: rest) = (acc, rest)
formTree (MkDirTree dirs files) (Cd dir :: rest) =
  let (subTree, rest') =
    formTree (fromMaybe emptyDir $ lookup dir dirs) rest in
  let acc' = MkDirTree (insert dir subTree dirs) files in
  formTree acc' rest'
formTree acc (Ls items :: rest) =
  formTree (foldl update acc items) rest
  where
    update : DirTree -> DirOutput -> DirTree
    update tree (DirLine name) =
      { dirs $= insert name emptyDir } tree
    update tree (FileLine name size) =
      { files $= ((name, size) ::) } tree

%hint
additive : Num a => Monoid a
additive = Additive

covering
dirSize : DirTree -> Nat
dirSize (MkDirTree dirs files) =
  foldMap dirSize (values dirs) + foldMap snd files

covering
filterDirs : (Nat -> Bool) -> DirTree -> List Nat
filterDirs p tree =
  let subDirs = toList tree.dirs >>= filterDirs p in
  let size = dirSize tree in
  if p size then size :: subDirs else subDirs

covering
solve1 : Input -> Nat
solve1 = sum . filterDirs (<= 100_000) . fst . formTree emptyDir

covering
part1 : Input ~> String
part1 = Mor $ ("Part 1: " ++) . show . solve1

-- Part 2

covering
solve2 : Input -> Nat
solve2 input =
  let tree = fst $ formTree emptyDir input in
  let totalSize = dirSize tree in
  let minSize = cast totalSize - 40_000_000 in
  min' $ totalSize ::: filterDirs (>= cast minSize) tree

covering
part2 : Input ~> String
part2 = Mor $ ("Part 2: " ++) . show . solve2

-- Plumbing

partial
main : IO ()
main = interact (unlines . (applyMor . sequence) [part1, part2] . parse)
