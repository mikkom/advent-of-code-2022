module AocUtils

import Data.List
import Data.List1
import Data.Vect
import System
import System.File

%default total

export
mapBoth : (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

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
