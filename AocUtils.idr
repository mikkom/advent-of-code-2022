module AocUtils

import Data.Vect
import System
import System.File

%default total

export
safeTail : List a -> List a
safeTail [] = []
safeTail (x :: xs) = xs

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
