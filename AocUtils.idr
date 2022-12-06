module AocUtils

import Data.List
import Data.List1
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
mapBoth : (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

export
mapPair : (a -> a') -> (b -> b') -> (a, b) -> (a', b')
mapPair f g (x, y) = (f x, g y)

export
safeTail : List a -> List a
safeTail = drop 1

export
partial
chunksOf : Nat -> List a -> List (List1 a)
chunksOf 0 xs = []
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
zipWithIndex : List a -> List (a, Nat)
zipWithIndex = loop 0
  where
    loop : Nat -> List a -> List (a, Nat)
    loop i [] = []
    loop i (x :: xs) = (x, i) :: loop (S i) xs

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
