module AocUtils

import Data.List
import Data.List1
import Data.Nat
import Data.Vect
import System
import System.File

%default total

export
partial
parsePair' : List a -> (a, a)
parsePair' [x, y] = (x, y)

export
partial
parsePair : List1 a -> (a, a)
parsePair = parsePair' . forget

infixl 10 !!
export
partial
(!!) : List a -> Nat -> a
(!!) (x :: xs) 0 = x
(!!) (x :: xs) (S k) = xs !! k

export
safeIndex : List a -> Nat -> Maybe a
safeIndex [] k = Nothing
safeIndex (x :: xs) 0 = Just x
safeIndex (x :: xs) (S k) = safeIndex xs k

export
mapBoth : (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

export
mapPair : (a -> a') -> (b -> b') -> (a, b) -> (a', b')
mapPair f g (x, y) = (f x, g y)

export
mapTwice : (a -> b) -> (a -> c) -> a -> (b, c)
mapTwice f g x = (f x, g x)

export
signum : Int -> Int
signum 0 = 0
signum x = if x > 0 then 1 else -1

export
covering
max' : Ord a => List1 a -> a
max' (x ::: []) = x
max' (x ::: y :: ys) = max' $ max x y ::: ys

export
safeTail : List a -> List a
safeTail = drop 1

export
covering
chunksOf : (n : Nat) -> NonZero n => List a -> List (List1 a)
chunksOf _ [] = []
chunksOf (S k) (x :: xs) = (x ::: take k xs) :: chunksOf (S k) (drop k xs)

export
windowed : (n : Nat) -> {m : Nat} -> Vect (n + m) a -> Vect (S m) (Vect n a)
windowed n xs {m = 0} = [prf xs]
  where
    prf : {n : Nat} -> (Vect (n + 0) a) -> Vect n a
    prf {n} xs = rewrite sym (plusZeroRightNeutral n) in xs
windowed n xs {m = S k} = take n xs :: windowed n (tail (prf xs))
  where
    prf : {n, k : Nat} -> Vect (n + S k) a -> Vect (S (n + k)) a
    prf {n} {k} xs = rewrite plusSuccRightSucc n k in xs

export
windowed' : (n : Nat) -> List a -> List (List a)
windowed' n [] = []
windowed' n ys @ (x :: xs) =
  if length ys < n then []
  else take n ys :: windowed' n xs

export
mapi : (Nat -> a -> b) -> List a -> List b
mapi f = loop 0
  where
    loop : Nat -> List a -> List b
    loop k [] = []
    loop k (x :: xs) = f k x :: loop (S k) xs

export
zipWithIndex : List a -> List (Nat, a)
zipWithIndex = mapi (,)

export
covering
everyNth : (n : Nat) -> NonZero n => List a -> List a
everyNth n xs =
  case splitAt (pred n) xs of
    (_, []) => []
    (_, (y :: ys)) => y :: (everyNth n ys)

export
fail : String -> IO ()
fail err = do putStrLn ("*** Error: " ++ err); exitFailure

%default covering

export
interact : HasIO io => (fn: String -> String) -> io ()
interact fn = do 
    input <- loop Nothing
    putStr $ fn input
  where 
    loop : Maybe String -> io String
    loop acc = do
      eof <- fEOF stdin
      if eof
        then pure (fromMaybe "" acc)
        else do
          line <- getLine
          case acc of
            Nothing => loop (Just line)
            (Just acc) => loop (Just $ acc ++ "\n" ++ line)
