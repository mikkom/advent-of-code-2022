module Interval

import Data.List

public export
record Interval where
  constructor MkInterval
  from : Int
  until : Int

export
Show Interval where
  show (MkInterval from until) = "[\{show from}, \{show until})"

export
length : Interval -> Nat
length (MkInterval from until) = cast $ until - from

public export
record Intervals where
  constructor MkIntervals
  intervals : List Interval

export
empty : Intervals
empty = MkIntervals []

export
singleton : Interval -> Intervals
singleton = MkIntervals . singleton

combine : Interval -> Interval -> List Interval
combine i@(MkInterval f u) i'@(MkInterval f' u') =
    if u < f' then [i, i']
    else if u' < f then [i', i]
    else [MkInterval (min f f') (max u u')]

reduce : List Interval -> List Interval
reduce = reverse . foldl insert [] . filter ((> 0) . length)
  where
    insert : (acc : List Interval) -> Interval -> List Interval
    insert [] curr = [curr]
    insert (prev :: acc) curr = reverse (combine prev curr) ++ acc

combineIntervals : Intervals -> Intervals -> Intervals
combineIntervals (MkIntervals is) (MkIntervals is') =
  MkIntervals $ reduce $ mergeBy (comparing from) is is'

export
Semigroup Intervals where
  (<+>) = combineIntervals

export
Monoid Intervals where
  neutral = empty

omit : Int -> Interval -> List Interval
omit x i@(MkInterval from until) =
  if x < from || x >= until then [i]
  else [MkInterval from x, MkInterval (x + 1) until]

export
remove : Int -> Intervals -> Intervals
remove x (MkIntervals is) = MkIntervals $ concatMap (omit x) is
